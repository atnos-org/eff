package org.atnos.eff.syntax.addon.fs2

import fs2.{Scheduler, Strategy, Task}
import org.atnos.eff.addon.fs2.{TaskEffect, TaskInterpretation, TimedTask}
import org.atnos.eff.{Cache, Eff, Fx, Member, Memoized, _}

import scala.util.Either

trait task {

  implicit final def toTaskOps[R, A](e: Eff[R, A]): TaskOps[R, A] = new TaskOps[R, A](e)

}

object task extends task

final class TaskOps[R, A](val e: Eff[R, A]) extends AnyVal {

  def runTaskMemo[U](cache: Cache)(implicit m: Member.Aux[Memoized, R, U], task: TimedTask |= U): Eff[U, A] =
    TaskEffect.runTaskMemo(cache)(e)

  def taskAttempt(implicit task: TimedTask /= R): Eff[R, Throwable Either A] =
    TaskInterpretation.taskAttempt(e)

  def taskMemo(key: AnyRef, cache: Cache)(implicit task: TimedTask /= R): Eff[R, A] =
    TaskInterpretation.taskMemo(key, cache, e)

  def runAsync(implicit strat: Strategy, sched: Scheduler, m: Member.Aux[TimedTask, R, NoFx]): Task[A] =
    TaskInterpretation.runAsync(e)

  def runSequential(implicit strat: Strategy, sched: Scheduler, m: Member.Aux[TimedTask, R, NoFx]): Task[A] =
    TaskInterpretation.runSequential(e)
}
