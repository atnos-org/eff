package org.atnos.eff.syntax.addon.monix

import java.util.Timer

import org.atnos.eff.{Fx, _}
import org.atnos.eff.addon.monix._
import _root_.monix.eval.Task
import scala.util.Either

trait task {

  implicit final def toTaskOps[R, A](e: Eff[R, A]): TaskOps[R, A] = new TaskOps[R, A](e)

}

final class TaskOps[R, A](val e: Eff[R, A]) extends AnyVal {

  def runTaskMemo[U](cache: Cache)(implicit m: Member.Aux[Memoized, R, U], task: TimedTask |= U): Eff[U, A] =
    TaskEffect.runTaskMemo(cache)(e)

  def taskAttempt(implicit task: TimedTask /= R): Eff[R, Throwable Either A] =
    TaskInterpretation.taskAttempt(e)

  def taskMemo(key: AnyRef, cache: Cache)(implicit task: TimedTask /= R): Eff[R, A] =
    TaskInterpretation.taskMemo(key, cache, e)

  def runAsync(implicit timer: Timer, ev: Eff[R, A] =:= Eff[Fx.fx1[TimedTask], A]): Task[A] =
    TaskInterpretation.runAsync(e)

  def runSequential(implicit timer: Timer, ev: Eff[R, A] =:= Eff[Fx.fx1[TimedTask], A]): Task[A] =
    TaskInterpretation.runSequential(e)
}


object task extends task
