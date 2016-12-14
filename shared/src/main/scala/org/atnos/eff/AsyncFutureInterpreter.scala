package org.atnos.eff

import java.util.concurrent.{ConcurrentHashMap, ExecutorService, ScheduledExecutorService, TimeUnit}

import cats._
import cats.implicits._
import org.atnos.eff.Async._
import org.atnos.eff.SubscribeEffect._
import org.atnos.eff.AsyncFutureInterpreter._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent._
import scala.util._

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
    fromFuture(future).flatten

  def fromFuture[R :_async, A](future: =>Future[A]): Eff[R, A] =
    subscribe[R, A](SimpleSubscribe(callback => future.onComplete {
      case Success(a) => callback(Right(a))
      case Failure(t) => callback(Left(t))

    }), None)

  def run[A](r: Async[A]): Future[A] =
    r match {
      case AsyncNow(a)     => Future.successful(a)
      case AsyncFailed(t)  => Future.failed(t)
      case AsyncDelayed(a) => Either.catchNonFatal(a.value).fold(Future.failed, Future.successful)
      case AsyncEff(e, to) => subscribeToFuture(e, to).detachA(FutureApplicative)
    }

  private val map: ConcurrentHashMap[Int, Eval[Future[Any]]] =
    new ConcurrentHashMap[Int, Eval[Future[Any]]]

  def subscribeToFutureNat(timeout: Option[FiniteDuration]) = new (Subscribe ~> Future) {
    def startFuture[X](subscribe: Subscribe[X]): (() => Future[X], Callback[X]) = {
      val promise: Promise[X] = Promise[X]()

      val callback = (ta: Throwable Either X) =>
        ta match {
          case Left(t)  => promise.failure(t); ()
          case Right(a) => promise.success(a); ()
        }

      (() => { Future(subscribe(callback)); promise.future }, callback)
    }

    def startTimeout(to: FiniteDuration, onTimeout: =>Unit): Unit =
      if (!to.isFinite || to.length >= 1) {
        val stop = new Runnable { def run: Unit = onTimeout }
        scheduledExecutorService.schedule(stop, to.toMillis, TimeUnit.MILLISECONDS)
        ()
      }

    def apply[X](subscribe: Subscribe[X]): Future[X] = {
      subscribe match {
        case SimpleSubscribe(s, Some((k, cache))) => cache.memo(k, apply(SimpleSubscribe(s)))
        case AttemptedSubscribe(s, Some((k, cache))) => cache.memo(k, apply(AttemptedSubscribe(s)))
        case _ =>
          timeout match {
            case None => startFuture(subscribe)._1()

            case Some(to) =>
              subscribe match {
                case SimpleSubscribe(_, _) =>
                  val (future, callback) = startFuture(subscribe)
                  startTimeout(to, { callback(Left(new TimeoutException)); () })
                  future()

                case AttemptedSubscribe(_, _) =>
                  val (future, callback) = startFuture(subscribe)
                  startTimeout(to, { callback(Right(Left(new TimeoutException))); () })
                  future()
              }

          }
      }

    }
  }

  def subscribeToFuture[A](e: Eff[Fx1[Subscribe], A], timeout: Option[FiniteDuration])(implicit m: Subscribe <= Fx1[Subscribe]): Eff[Fx1[Future], A] =
    interpret.transform[Fx1[Subscribe], Fx1[Future], NoFx, Subscribe, Future, A](e, subscribeToFutureNat(timeout))

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

  def create(implicit ec: ExecutionContext): AsyncFutureInterpreter =
    fromExecutionContext(ec)

  /** create an AsyncFutureService but do not evaluate the execution context yet */
  def fromExecutionContext(ec: =>ExecutionContext): AsyncFutureInterpreter =
    fromExecutionEnv(ExecutorServices.fromExecutionContext(ec))

  /** create an AsyncFutureService but do not evaluate the execution context yet */
  def fromExecutionEnv(ee: ExecutorServices): AsyncFutureInterpreter =
    AsyncFutureInterpreter(ee)

  def FutureApplicative(implicit ec: ExecutionContext): Applicative[Future] = new Applicative[Future] {
    def pure[A](x: A): Future[A] =
      Future.successful(x)

    def ap[A, B](ff: Future[A => B])(fa: Future[A]): Future[B] = {
      fa.zip(ff).map { case (a, f) => f(a) }
    }
  }

}
