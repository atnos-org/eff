package org.atnos.eff

import cats.data.Xor
import _root_.monix.eval.Task

package object monix extends TaskCreation with TaskInterpretation {
  implicit class TaskOps[R, A](e: Eff[R, A]) {
    def attemptTask(implicit task: Task /= R): Eff[R, Throwable Xor A] =
      TaskInterpretation.attempt(e)
  }
}
