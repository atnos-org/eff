package org.atnos.eff

abstract class AugmentCompanion { self: Augment.type =>
  final def lift[T[_], O[_]](f: [X] => T[X] => O[Unit]): Augment[T, O] =
    new Augment[T, O] {
      override def apply[Y](tx: T[Y]): O[Unit] =
        f[Y](tx)
    }
}
