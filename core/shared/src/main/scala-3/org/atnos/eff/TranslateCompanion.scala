package org.atnos.eff

abstract class TranslateCompanion { self: Translate.type =>
  final def lift[T[_], U](f: [X] => T[X] => Eff[U, X]): Translate[T, U] =
    new Translate[T, U] {
      override def apply[Y](kv: T[Y]): Eff[U, Y] =
        f[Y](kv)
    }
}
