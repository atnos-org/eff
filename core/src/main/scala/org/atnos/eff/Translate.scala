package org.atnos.eff

/**
 * trait for translating one effect into other ones in the same stack
 */
trait Translate[T[_], U] {
  def apply[X](kv: T[X]): Eff[U, X]
}

object Translate {
  def lift[T[_], U](f: [X] => T[X] => Eff[U, X]): Translate[T, U] =
    new Translate[T, U] {
      override def apply[Y](kv: T[Y]): Eff[U, Y] =
        f[Y](kv)
    }
}
