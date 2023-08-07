package org.atnos.eff

private[eff] object EffCompat {

  extension [A](self: A) {
    inline def cast[B]: B = self.asInstanceOf[B]
  }

}
