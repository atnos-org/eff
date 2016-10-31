package org.atnos.eff

import _root_.monix.eval.Task

import scala.concurrent.duration.FiniteDuration

package object monix extends TaskCreation with TaskInterpretation {
  implicit class TaskOps[R, A](e: Eff[R, A]) {
    def attemptTask(implicit task: Task /= R): Eff[R, Throwable Either A] =
      TaskInterpretation.attempt(e)

    def withTimeout(duration: FiniteDuration)(implicit task: Task /= R): Eff[R, A] =
      TaskInterpretation.withTimeout(e)(duration)
  }
}
