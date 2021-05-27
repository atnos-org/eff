package org.atnos.eff

private[eff] object EffCompat {
  inline def cast[A, B](inline value: A): B =
    value.asInstanceOf[B]
}
