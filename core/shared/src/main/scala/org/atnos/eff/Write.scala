package org.atnos.eff

trait Write[T[_], O] {
  def apply[X](tx: T[X]): O
}

object Write {
  def lift[T[_], O](f: [X] => T[X] => O): Write[T, O] =
    new Write[T, O] {
      override def apply[Y](tx: T[Y]): O =
        f[Y](tx)
    }
}
