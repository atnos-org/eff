package org.atnos.eff.syntax

import org.atnos.eff._

import scala.concurrent.{ExecutionContext, Future}

trait future {

  implicit final def toFutureOps[R, A](e: Eff[R, A]): FutureOps[R, A] = new FutureOps[R, A](e)

}

object future extends future


final class FutureOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def futureAttempt(implicit future: TimedFuture /= R): Eff[R, Throwable Either A] =
    FutureInterpretation.futureAttempt(e)

  def futureMemo(key: AnyRef, cache: Cache)(implicit future: TimedFuture /= R): Eff[R, A] =
    FutureInterpretation.futureMemo(key, cache, e)

  def runAsync(implicit scheduler: Scheduler, exc: ExecutionContext, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    FutureInterpretation.runAsync(e)

  def runSequential(implicit scheduler: Scheduler, exc: ExecutionContext, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    FutureInterpretation.runSequential(e)
}


