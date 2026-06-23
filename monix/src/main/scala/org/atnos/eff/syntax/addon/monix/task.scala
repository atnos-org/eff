package org.atnos.eff.syntax.addon.monix

import _root_.monix.eval.Task
import monix.execution.Scheduler
import org.atnos.eff.*
import org.atnos.eff.addon.monix.*
import scala.concurrent.duration.FiniteDuration

trait task {

  given taskExtension: AnyRef with {

    extension [R, A](e: Eff[R, A]) {

      def asyncBoundary(using Task |= R): Eff[R, A] =
        e.flatMap(a => TaskEffect.asyncBoundary.map(_ => a))

      def asyncBoundary(s: Scheduler)(using Task |= R): Eff[R, A] =
        e.flatMap(a => TaskEffect.asyncBoundary(s).map(_ => a))

      def taskAttempt(using Task /= R): Eff[R, Either[Throwable, A]] =
        TaskInterpretation.taskAttempt(e)

      def taskMemo(key: AnyRef, cache: Cache)(using Task /= R): Eff[R, A] =
        TaskInterpretation.taskMemo(key, cache, e)

      def runAsync(using Member.Aux[Task, R, NoFx]): Task[A] =
        TaskInterpretation.runAsync(e)

      def runSequential(using Member.Aux[Task, R, NoFx]): Task[A] =
        TaskInterpretation.runSequential(e)

      def retryUntil(condition: A => Boolean, durations: List[FiniteDuration])(using Task |= R): Eff[R, A] =
        TaskCreation.retryUntil(e, condition, durations)
    }
  }
}

object task extends task
