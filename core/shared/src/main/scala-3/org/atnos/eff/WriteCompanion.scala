package org.atnos.eff

abstract class WriteCompanion { self: Write.type =>
  final def lift[T[_], O](f: [X] => T[X] => O): Write[T, O] =
    new Write[T, O] {
      override def apply[Y](tx: T[Y]): O =
        f[Y](tx)
    }
}
