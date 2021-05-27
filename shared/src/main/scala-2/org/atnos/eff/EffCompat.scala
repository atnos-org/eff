package org.atnos.eff

private[eff] object EffCompat {
  @inline def cast[A](value: A): A = value
}
