package org.atnos.eff

import java.util.concurrent.{ExecutorService, ScheduledExecutorService, TimeUnit}

import cats._
import cats.implicits._
import org.atnos.eff.AsyncFutureInterpreter._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent._
import scala.util._
import scala.util.control.NonFatal

case class AsyncFutureInterpreter(executors: ExecutorServices) extends AsyncInterpreter[Future] { outer =>

  implicit lazy val executorService: ExecutorService =
    executors.executorService

  implicit lazy val scheduledExecutorService: ScheduledExecutorService =
    executors.scheduledExecutorService

  implicit lazy val executionContext: ExecutionContext =
    executors.executionContext

  def runAsync[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    run(e.detachA(ApplicativeAsync))

  def runSequential[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    run(e.detach)

  def suspend[R :_async, A](future: =>Future[Eff[R, A]]): Eff[R, A] =
    send[Async, R, Eff[R, A]](AsyncCallback(callback => future.onComplete {
      case Success(a) => callback(Right(a))
      case Failure(t) => callback(Left(t))

    }, None)).flatten

  def run[A](r: Async[A]): Future[A] =
    r match {
      case AsyncNow(a)    => Future.successful(a)
      case AsyncFailed(t) => Future.failed(t)

      case AsyncDelayed(a, to) =>
        to match {
          case Some(timeout) => withTimeout(Future(a()), timeout)
          case None          => Either.catchNonFatal(a()).fold(Future.failed, Future.successful)
        }

      case AsyncCallback(register, to)  =>
        val promise: Promise[A] = Promise[A]()
        val callback = (ta: Throwable Either A) =>
          ta match {
            case Left(t)  => promise.failure(t); ()
            case Right(a) => promise.success(a); ()
          }
        register(callback)
        to match {
          case Some(timeout) => withTimeout(promise.future, timeout)
          case None          => promise.future
        }

    }

  def withTimeout[A](f: Future[A], timeout: FiniteDuration): Future[A] = {
    if (timeout.isFinite && timeout.length < 1) try f catch { case NonFatal(t) => Future.failed(t) }
    else {
      val p = Promise[A]()
      val r = new Runnable {
        def run: Unit = {
          p completeWith { try f catch { case NonFatal(t) => Future.failed(t) } }
          ()
        }
      }
      scheduledExecutorService.schedule(r, timeout.toMillis, TimeUnit.MILLISECONDS)
      p.future
    }
  }

  implicit final def toRunAsyncFutureOps[A](e: Eff[Fx.fx1[Async], A]): RunAsyncFutureOps[A] =
    new RunAsyncFutureOps[A](e)

  final class RunAsyncFutureOps[A](val e: Eff[Fx.fx1[Async], A]) {

    def runAsyncFuture: Future[A] =
      outer.runAsync(e)

    def runAsyncSequential: Future[A] =
      outer.runSequential(e)
  }

}

trait AsyncInterpreter[F[_]] {

  def runAsync[A](e: Eff[Fx.fx1[Async], A]): F[A]

  def runSequential[A](e: Eff[Fx.fx1[Async], A]): F[A]

}


object AsyncFutureInterpreter {
  type Callback[A] = (Throwable Either A) => Unit

  def create(implicit ec: ExecutionContext): AsyncFutureInterpreter =
    fromExecutionContext(ec)

  /** create an AsyncFutureService but do not evaluate the execution context yet */
  def fromExecutionContext(ec: =>ExecutionContext): AsyncFutureInterpreter =
    fromExecutionEnv(ExecutorServices.fromExecutionContext(ec))

  /** create an AsyncFutureService but do not evaluate the execution context yet */
  def fromExecutionEnv(ee: ExecutorServices): AsyncFutureInterpreter =
    AsyncFutureInterpreter(ee)

  def ApplicativeAsync: Applicative[Async] = new Applicative[Async] {

    def pure[A](a: A) = AsyncNow(a)

    def ap[A, B](ff: Async[A => B])(fa: Async[A]): Async[B] =
      fa match {
        case AsyncNow(a) =>
          ff match {
            case AsyncNow(f) =>
              AsyncNow(f(a))

            case AsyncFailed(t) =>
              AsyncFailed(t)

            case AsyncDelayed(f, to) =>
              AsyncDelayed(() => f()(a), to)

            case AsyncCallback(r, to) =>
              AsyncCallback((callback: Callback[B]) => r((ta: Either[Throwable, A => B]) => callback(ta.map(_(a)))), to)
          }

        case AsyncFailed(t) => AsyncFailed(t)

        case AsyncDelayed(a, to) =>
          ff match {
            case AsyncNow(f) =>
              AsyncDelayed(() => f(a()), to)

            case AsyncFailed(t) =>
              AsyncFailed(t)

            case AsyncDelayed(f, tof) =>
              AsyncDelayed(() => f()(a()), (to |@| tof).map(_ min _))

            case AsyncCallback(r, tof) =>
              AsyncCallback((callback: Callback[B]) => r((ta: Either[Throwable, A => B]) => callback(ta.map(_(a())))), (to |@| tof).map(_ min _))
          }

        case AsyncCallback(r, to) =>
          ff match {
            case AsyncNow(f) =>
              AsyncCallback((callback: Callback[B]) => r((ta: Either[Throwable, A]) => callback(ta.map(f))), to)

            case AsyncFailed(t) =>
              AsyncFailed(t)

            case AsyncDelayed(f, tof) =>
              AsyncCallback((callback: Callback[B]) => r((ta: Either[Throwable, A]) => callback(ta.map(f()))), (to |@| tof).map(_ min _))

            case AsyncCallback(rf, tof) =>
              AsyncCallback((callback: Callback[B]) => rf((taf: Either[Throwable, A => B]) =>
                r((ta: Either[Throwable, A]) => callback(Apply[Either[Throwable, ?]].ap(taf)(ta)))), (to |@| tof).map(_ min _))

          }
      }

    override def toString = "Applicative[Async]"
  }

  implicit final def MonadAsync: Monad[Async] = new Monad[Async] {
    def pure[A](a: A) = AsyncNow(a)

    def flatMap[A, B](aa: Async[A])(af: A => Async[B]): Async[B] =
      aa match {
        case AsyncNow(a) => af(a)
        case AsyncDelayed(a, to) => af(a())
        case AsyncFailed(t)  => AsyncFailed(t)

        case AsyncCallback(r, to) =>
          def callbackA(callbackB: Callback[B]): Callback[A] = (ta: Either[Throwable, A]) =>
            ta match {
              case Left(t) => callbackB(Left(t))
              case Right(a) =>
                af(a) match {
                  case AsyncNow(b) => callbackB(Right(b))
                  case AsyncFailed(t) => callbackB(Left(t))
                  case AsyncDelayed(r1, _) => callbackB(Right(r1()))
                  case AsyncCallback(r1, _) => r1(callbackB)
                }
            }

          AsyncCallback((callbackB: Callback[B]) => r(callbackA(callbackB)), to)
      }

    def tailRecM[A, B](a: A)(f: A => Async[Either[A, B]]): Async[B] =
      f(a).flatMap {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => pure(b)
      }

    override def toString = "Monad[AsyncFuture]"
  }

  def FutureApplicative(implicit ec: ExecutionContext): Applicative[Future] = new Applicative[Future] {
    def pure[A](x: A): Future[A] =
      Future.successful(x)

    def ap[A, B](ff: Future[A => B])(fa: Future[A]): Future[B] = {
      fa.zip(ff).map { case (a, f) => f(a) }
    }
  }

}

case class AsyncNow[A](a: A) extends AnyVal with Async[A]

case class AsyncFailed[A](t: Throwable) extends AnyVal with Async[A]

case class AsyncDelayed[A](run: () => A, timeout: Option[FiniteDuration]) extends Async[A]

case class AsyncCallback[A](register: ((Throwable Either A) => Unit) => Unit, timeout: Option[FiniteDuration]) extends Async[A]

sealed trait Async[+A] extends Any

object Async {
  type Callback[A] = (Throwable Either A) => Unit

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    send[Async, R, A](AsyncNow[A](a))

  def asyncFail[R :_async, A](t: Throwable): Eff[R, A] =
    send[Async, R, A](AsyncFailed(t))

  def asyncDelay[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[Async, R, A](AsyncDelayed(() => a, timeout))

  def asyncFork[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[Async, R, A](AsyncCallback(f => f(Either.catchNonFatal(a)), timeout))

  def fork[R :_async, A](a: =>Async[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    asyncDelay[R, Eff[R, A]] {
      a match {
        case AsyncDelayed(r, to) => send[Async, R, A](AsyncCallback(f => f(Right(r())), to))
        case other               => send[Async, R, A](other)
      }
    }.flatten

  def async[R :_async, A](register: ((Throwable Either A) => Unit) => Unit, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[Async, R, A](AsyncCallback(register, timeout))

  def attempt[A](a: Async[A]): Async[Throwable Either A] =
    a match {
      case AsyncNow(v) => AsyncNow(Right(v))
      case AsyncFailed(t) => AsyncNow(Left(t))
      case AsyncDelayed(r, to) => AsyncDelayed(() => Either.catchNonFatal(r()), to)
      case AsyncCallback(r, to) =>
        AsyncCallback((callback: Callback[Throwable Either A]) => r(ta => callback(Right(ta))), to)
    }

}
