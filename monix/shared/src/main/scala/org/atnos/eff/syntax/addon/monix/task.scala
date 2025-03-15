package org.atnos.eff.syntax.addon.monix

import _root_.monix.eval.Task
import monix.execution.Scheduler
import org.atnos.eff.*
import org.atnos.eff.addon.monix.*
import scala.concurrent.duration.FiniteDuration

trait task {

  implicit final def toTaskOps[R, A](e: Eff[R, A]): TaskOps[R, A] = new TaskOps[R, A](e)

}

final class TaskOps[R, A](private val e: Eff[R, A]) extends AnyVal {

  def asyncBoundary(implicit task: Task |= R): Eff[R, A] =
    e.flatMap(a => TaskEffect.asyncBoundary.map(_ => a))

  def asyncBoundary(s: Scheduler)(implicit task: Task |= R): Eff[R, A] =
    e.flatMap(a => TaskEffect.asyncBoundary(s).map(_ => a))

  def taskAttempt(implicit task: Task /= R): Eff[R, Either[Throwable, A]] =
    TaskInterpretation.taskAttempt(e)

  def taskMemo(key: AnyRef, cache: Cache)(implicit task: Task /= R): Eff[R, A] =
    TaskInterpretation.taskMemo(key, cache, e)

  def runAsync(implicit m: Member.Aux[Task, R, NoFx]): Task[A] =
    TaskInterpretation.runAsync(e)

  def runSequential(implicit m: Member.Aux[Task, R, NoFx]): Task[A] =
    TaskInterpretation.runSequential(e)

  def retryUntil(condition: A => Boolean, durations: List[FiniteDuration])(implicit task: Task |= R): Eff[R, A] =
    TaskCreation.retryUntil(e, condition, durations)
}

object task extends task
