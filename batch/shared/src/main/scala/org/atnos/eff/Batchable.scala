package org.atnos.eff

trait Batchable[T[_]] {
  type Z
  type E
  def distribute(z: Z): List[E]
  def batch[X, Y](t1: T[X], t2: T[Y]): Option[T[Z]]
}
