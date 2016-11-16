package org.atnos.eff

import java.util.concurrent.{ExecutorService, ScheduledExecutorService, TimeUnit}

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
    subscribe[R, A](callback => future.onComplete {
      case Success(a) => callback(Right(a))
      case Failure(t) => callback(Left(t))

    }, None)

  def run[A](r: Async[A]): Future[A] =
    r match {
      case AsyncNow(a)         => Future.successful(a)
      case AsyncFailed(t)      => Future.failed(t)
      case AsyncDelayed(a, to) => withTimeout(Either.catchNonFatal(a.value).fold(Future.failed, Future.successful), to)
      case AsyncEff(e, to)     => subscribeToFuture(e, to).detachA(FutureApplicative)
    }

  def subscribeToFutureNat(timeout: Option[FiniteDuration]) = new (Subscribe ~> Future) {
    def apply[X](subscribe: Subscribe[X]): Future[X] = {
      val promise: Promise[X] = Promise[X]()
      val callback = (ta: Throwable Either X) =>
        ta match {
          case Left(t)  => promise.failure(t); ()
          case Right(a) => promise.success(a); ()
        }

      withTimeout(FutureApplicative.product(Future(subscribe(callback)), promise.future).map(_._2), timeout)
    }
  }

  def subscribeToFuture[A](e: Eff[Fx1[Subscribe], A], timeout: Option[FiniteDuration])(implicit m: Subscribe <= Fx1[Subscribe]): Eff[Fx1[Future], A] =
    interpret.transform[Fx1[Subscribe], Fx1[Future], NoFx, Subscribe, Future, A](e, subscribeToFutureNat(timeout))

  def withTimeout[A](future: =>Future[A], timeout: Option[FiniteDuration]): Future[A] =
    timeout match {
      case None => future
      case Some(to) =>
        lazy val attemptFuture = Either.catchNonFatal(future).fold(Future.failed, identity)

        if (to.isFinite && to.length < 1) attemptFuture
        else {
          val p = Promise[A]()
          val r = new Runnable { def run: Unit = { p.completeWith(attemptFuture); () } }
          scheduledExecutorService.schedule(r, to.toMillis, TimeUnit.MILLISECONDS)
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
