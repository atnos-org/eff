package org.atnos.eff

import cats.data.*
import org.atnos.eff.Eff.send

trait WriterCreation {

  /** write a given value */
  def tell[R, O](o: O)(using Writer[O, *] |= R): Eff[R, Unit] =
    send[Writer[O, *], R, Unit](Writer(o, ()))

}

object WriterCreation extends WriterCreation
