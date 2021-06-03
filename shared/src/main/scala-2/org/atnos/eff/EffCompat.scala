package org.atnos.eff

private[eff] object EffCompat {

  private[eff] implicit class CastOps[A](private val self: A) extends AnyVal {
    @inline def cast[B]: A = self
  }

}
