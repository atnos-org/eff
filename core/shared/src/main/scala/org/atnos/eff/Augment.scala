package org.atnos.eff

trait Augment[T[_], O[_]] {
  def apply[X](tx: T[X]): O[Unit]
}

object Augment {
  def lift[T[_], O[_]](f: [X] => T[X] => O[Unit]): Augment[T, O] =
    new Augment[T, O] {
      override def apply[Y](tx: T[Y]): O[Unit] =
        f[Y](tx)
    }
}
