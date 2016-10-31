package org.atnos.eff.syntax

import org.atnos.eff._

object batch extends batch

trait batch {

  implicit class BatchOps[R, A](e: Eff[R, A]) {

    def batch[T[_]](implicit batchable: Batchable[T], member: T /= R): Eff[R, A] =
      Batch.batch[R, T, A](e)(batchable, member)

  }

}
