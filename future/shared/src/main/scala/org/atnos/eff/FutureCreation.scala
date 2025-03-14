package org.atnos.eff

import org.atnos.eff.concurrent.Scheduler
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

object FutureCreation extends FutureCreation

trait FutureCreation extends FutureTypes {

  final def fromFutureWithExecutors[R: _future, A](c: (Scheduler, ExecutionContext) => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    Eff.send[TimedFuture, R, A](TimedFuture(c, timeout))

  final def fromFuture[R: _future, A](c: => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    Eff.send[TimedFuture, R, A](TimedFuture((_, _) => c, timeout))

  final def futureFail[R: _future, A](t: Throwable): Eff[R, A] =
    Eff.send[TimedFuture, R, A](TimedFuture((_, _) => Future.failed(t)))

  final def futureFromEither[R: _future, A](e: Throwable Either A): Eff[R, A] =
    e.fold(futureFail[R, A], Eff.pure[R, A])

  final def futureDelay[R: _future, A](a: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    Eff.send[TimedFuture, R, A](TimedFuture((_, ec) => Future(a)(using ec), timeout))

  final def futureFork[R: _future, A](a: => A, ec: ExecutionContext, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    Eff.send[TimedFuture, R, A](TimedFuture((_, _) => Future(a)(using ec), timeout))

  final def futureDefer[R: _future, A](a: => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    Eff.send[TimedFuture, R, A](TimedFuture((_, _) => a, timeout))

  def retryUntil[R: _future, A](e: Eff[R, A], condition: A => Boolean, durations: List[FiniteDuration]): Eff[R, A] =
    Eff.retryUntil(e, condition, durations, d => waitFor(d))

  def waitFor[R: _future](duration: FiniteDuration): Eff[R, Unit] =
    Eff.send(TimedFuture((scheduler, _) => scheduler.delay(duration)))
}
