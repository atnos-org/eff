package org.atnos.eff.syntax.addon.scalaz

import org.atnos.eff.addon.scalaz.concurrent.{TaskCreation, TaskEffect, TaskInterpretation, TimedTask}
import org.atnos.eff._

import scala.util.Either
import _root_.scalaz.concurrent.Task
import scala.concurrent.duration.FiniteDuration

trait task {

  implicit final def toTaskOps[R, A](e: Eff[R, A]): TaskOps[R, A] = new TaskOps[R, A](e)

}

object task extends task


final class TaskOps[R, A](private val e: Eff[R, A]) extends AnyVal {

  def runTaskMemo[U](cache: Cache)(implicit m: Member.Aux[Memoized, R, U], task: TimedTask |= U): Eff[U, A] =
    TaskEffect.runTaskMemo(cache)(e)

  def taskAttempt(implicit task: TimedTask /= R): Eff[R, Throwable Either A] =
    TaskInterpretation.taskAttempt(e)

  def taskMemo(key: AnyRef, cache: Cache)(implicit async: TimedTask /= R): Eff[R, A] =
    TaskInterpretation.taskMemo(key, cache, e)

  def runAsync(implicit es: ExecutorServices, m: Member.Aux[TimedTask, R, NoFx]): Task[A] =
    TaskInterpretation.runAsync(e)(es, m)

  def runSequential(implicit es: ExecutorServices, m: Member.Aux[TimedTask, R, NoFx]): Task[A] =
    TaskInterpretation.runSequential(e)(es, m)

  def retryUntil(condition: A => Boolean, durations: List[FiniteDuration])(implicit task: TimedTask |= R): Eff[R, A] =
    TaskCreation.retryUntil(e, condition, durations)


}
