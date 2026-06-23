package org.atnos.eff.syntax

import org.atnos.eff.*
import org.atnos.eff.concurrent.Scheduler
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

trait future {

  given futureExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {
      def futureAttempt(using TimedFuture /= R): Eff[R, Either[Throwable, A]] =
        FutureInterpretation.futureAttempt(e)

      def futureMemo(key: AnyRef, cache: Cache)(using TimedFuture /= R): Eff[R, A] =
        FutureInterpretation.futureMemo(key, cache, e)

      def runAsync(using scheduler: Scheduler, exc: ExecutionContext, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
        FutureInterpretation.runAsync(e)

      def runAsyncOn(executorServices: ExecutorServices)(using Member.Aux[TimedFuture, R, NoFx]): Future[A] =
        FutureInterpretation.runAsyncOn(executorServices)(e)

      def runSequentialOn(executorServices: ExecutorServices)(using Member.Aux[TimedFuture, R, NoFx]): Future[A] =
        FutureInterpretation.runSequentialOn(executorServices)(e)

      def runSequential(using Scheduler, ExecutionContext, Member.Aux[TimedFuture, R, NoFx]): Future[A] =
        FutureInterpretation.runSequential(e)

      def retryUntil(condition: A => Boolean, durations: List[FiniteDuration])(using TimedFuture |= R): Eff[R, A] =
        FutureCreation.retryUntil(e, condition, durations)
    }
  }
}

object future extends future
