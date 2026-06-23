package org.atnos.eff

import org.atnos.eff.Eff.send

trait OptionCreation {
  type _Option[R] = Option <= R
  type _option[R] = Option |= R

  /** create an Option effect from a single Option value */
  def fromOption[R: _option, A](o: Option[A]): Eff[R, A] =
    send[Option, R, A](o)

  /** no value returned */
  def none[R: _option, A]: Eff[R, A] =
    send[Option, R, A](None)

  /** a value is returned */
  def some[R: _option, A](a: A): Eff[R, A] =
    send[Option, R, A](Some(a))
}

object OptionCreation extends OptionCreation
