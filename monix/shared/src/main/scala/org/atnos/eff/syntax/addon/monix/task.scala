package org.atnos.eff.syntax.addon.monix

import org.atnos.eff._
import org.atnos.eff.addon.monix._
import _root_.monix.eval.Task
import scala.util.Either

trait task {

  implicit final def toTaskOps[R, A](e: Eff[R, A]): TaskOps[R, A] = new TaskOps[R, A](e)

}

final class TaskOps[R, A](val e: Eff[R, A]) extends AnyVal {

  def runTaskMemo[U](cache: Cache)(implicit m: Member.Aux[Memoized, R, U], task: Task |= U): Eff[U, A] =
    TaskEffect.runTaskMemo(cache)(e)

  def taskAttempt(implicit task: Task /= R): Eff[R, Throwable Either A] =
    TaskInterpretation.taskAttempt(e)

  def taskMemo(key: AnyRef, cache: Cache)(implicit task: Task /= R): Eff[R, A] =
    TaskInterpretation.taskMemo(key, cache, e)

  def runAsync(implicit m: Member.Aux[Task, R, NoFx]): Task[A] =
    TaskInterpretation.runAsync(e)

  def runSequential(implicit m: Member.Aux[Task, R, NoFx]): Task[A] =
    TaskInterpretation.runSequential(e)
}


object task extends task
