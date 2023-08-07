package org.atnos.eff.syntax

import org.atnos.eff._

object batch extends batch

trait batch {
  implicit final def toBatchOps[R, A](e: Eff[R, A]): BatchOps[R, A] = new BatchOps(e)
}

final class BatchOps[R, A](private val e: Eff[R, A]) extends AnyVal {

  def batch[T[_]](implicit batchable: Batchable[T], member: T /= R): Eff[R, A] =
    Batch.batch[R, T, A](e)(batchable, member)

}
