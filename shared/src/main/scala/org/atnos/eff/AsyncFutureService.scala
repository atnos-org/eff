package org.atnos.eff

import java.util.concurrent.{ExecutorService, ScheduledExecutorService, TimeUnit}

import cats._
import cats.implicits._
import org.atnos.eff.AsyncFutureService._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

case class AsyncFutureService(executors: ExecutorServices) extends AsyncService {

  implicit lazy val executorService: ExecutorService =
    executors.executorService

  implicit lazy val scheduledExecutorService: ScheduledExecutorService =
    executors.scheduledExecutorService

  implicit lazy val executionContext: ExecutionContext =
    executors.executionContext

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    send[Async, R, A](AsyncFutureNow[A](a))

  def asyncFail[R :_async](t: Throwable): Eff[R, Unit] =
    send[Async, R, Unit](AsyncFutureFailed(t))

  def asyncDelay[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[Async, R, Eff[R, A]](AsyncFutureDelayed(() => {
      timeout match {
        case None =>
          send[Async, R, A](AsyncFutureDelayed(() => a))

        case Some(to) =>
          send[Async, R, A](AsyncFutureDelayed(() => Await.result(Future(a), to)))
      }
    })).flatten

  def suspend[R :_async, A](future: =>Future[Eff[R, A]]): Eff[R, A] =
    send[Async, R, Eff[R, A]](AsyncFuture(executionContext, { _ => future })).flatten

  def asyncFork[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    fork(AsyncFuture(executionContext, {ec1 => Future(a)(ec1)}), timeout)

  def fork[R :_async, A](a: =>Async[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    timeout match {
      case None => asyncDelay(send(a)).flatten
      case Some(to) =>
        a match {
          case AsyncFutureNow(r)     => AsyncFutureNow(r)
          case AsyncFutureFailed(t)  => AsyncFutureFailed(t)
          case AsyncFutureDelayed(r) => AsyncFutureDelayed(r)
          case AsyncFuture(ec, f)    => AsyncFuture(ec, { ec1 => withTimeout(f(ec1), to) })
        }
        asyncDelay(send(a)).flatten
    }

  def withTimeout[A](f: Future[A], timeout: FiniteDuration): Future[A] = {
    if (timeout.isFinite && timeout.length < 1) {
      try f catch { case NonFatal(t) => Future.failed(t) }
    } else {
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

}

trait AsyncFutureServiceInterpretation {

  def runAsyncFuture[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    e.detachA(ApplicativeAsync) match {
      case AsyncFutureNow(a)     => Future.successful(a)
      case AsyncFutureDelayed(a) => Either.catchNonFatal(a()).fold(Future.failed, Future.successful)
      case AsyncFutureFailed(t)  => Future.failed(t)
      case AsyncFuture(ec, run)  => run(ec)
    }

  def runFuture[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    e.detach match {
      case AsyncFutureNow(a)     => Future.successful(a)
      case AsyncFutureDelayed(a) => Either.catchNonFatal(a()).fold(Future.failed, Future.successful)
      case AsyncFutureFailed(t)  => Future.failed(t)
      case AsyncFuture(ec, run)  => run(ec)
    }

  implicit class RunAsyncFutureOps[A](e: Eff[Fx.fx1[Async], A]) {
    def runAsyncFuture: Future[A] =
      AsyncFutureServiceInterpretation.runAsyncFuture(e)

    def runFuture: Future[A] =
      AsyncFutureServiceInterpretation.runFuture(e)
  }

}

object AsyncFutureServiceInterpretation extends AsyncFutureServiceInterpretation

object AsyncFutureService {

  def create(implicit ec: ExecutionContext): AsyncFutureService =
    fromExecutionContext(ec)

  /** create an AsyncFutureService but do not evaluate the execution context yet */
  def fromExecutionContext(ec: =>ExecutionContext): AsyncFutureService =
    fromExecutionEnv(ExecutorServices.fromExecutionContext(ec))

  /** create an AsyncFutureService but do not evaluate the execution context yet */
  def fromExecutionEnv(ee: ExecutorServices): AsyncFutureService =
    AsyncFutureService(ee)

  def ApplicativeAsync: Applicative[Async] = new Applicative[Async] {

    def pure[A](a: A) = AsyncFutureNow(a)

    def ap[A, B](ff: Async[A => B])(fa: Async[A]): Async[B] =
      fa match {
        case AsyncFutureNow(a) =>
          ff match {
            case AsyncFutureNow(f)     => AsyncFutureNow(f(a))
            case AsyncFutureFailed(t)  => AsyncFutureFailed(t)
            case AsyncFutureDelayed(f) => AsyncFutureDelayed(() => f()(a))
            case AsyncFuture(ecf, f)   => AsyncFuture(ecf, { implicit ec => f(ec).map(_(a))})
          }

        case AsyncFutureFailed(t) => AsyncFutureFailed(t)

        case AsyncFutureDelayed(a) =>
          ff match {
            case AsyncFutureNow(f)     => AsyncFutureDelayed(() => f(a()))
            case AsyncFutureFailed(t)  => AsyncFutureFailed(t)
            case AsyncFutureDelayed(f) => AsyncFutureDelayed(() => f()(a()))
            case AsyncFuture(ecf, f)   => AsyncFuture(ecf, { implicit ec => f(ec).map(_(a()))})
          }

        case AsyncFuture(eca, a) =>
          AsyncFuture(eca, { implicit ec =>
            val app = FutureApplicative
            ff match {
              case AsyncFutureNow(f)     => a(ec).map(f)
              case AsyncFutureFailed(t)  => Future.failed(t)
              case AsyncFutureDelayed(f) => a(ec).map(x => f()(x))
              case AsyncFuture(ecf, f)   => app.ap(f(ecf))(a(ec))
            }
          })
      }

    override def toString = "Applicative[AsyncFuture]"
  }

  implicit def MonadAsync: Monad[Async] = new Monad[Async] {
    def pure[A](a: A) = AsyncFutureNow(a)

    def flatMap[A, B](aa: Async[A])(af: A => Async[B]): Async[B] =
      aa match {
        case AsyncFutureNow(a)     => af(a)
        case AsyncFutureDelayed(a) => af(a())
        case AsyncFutureFailed(t)  => AsyncFutureFailed(t)

        case AsyncFuture(eca, fa) =>
          AsyncFuture(eca, { implicit ec =>
            fa(ec).flatMap { a =>
              af(a) match {
                case AsyncFutureNow(b)          => Future.successful(b)
                case AsyncFutureFailed(t)       => Future.failed(t)
                case AsyncFutureDelayed(b)      => try Future(b()) catch { case NonFatal(t) => Future.failed(t) }
                case AsyncFuture(ecb, fb) => fb(ecb)
              }
            }})
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

case class AsyncFutureNow[A](a: A) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncFutureNow(Right(a))
}

case class AsyncFutureFailed[A](t: Throwable) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncFutureNow(Left(t))
}

case class AsyncFutureDelayed[A](run: () => A) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncFutureDelayed(() => Either.catchNonFatal(run()))
}

case class AsyncFuture[A](ec: ExecutionContext, run: ExecutionContext => Future[A]) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncFuture(ec, { implicit ec =>
      run(ec).map(a => Either.right[Throwable, A](a))(ec) recover { case NonFatal(t) => Left(t) }
    })
}
