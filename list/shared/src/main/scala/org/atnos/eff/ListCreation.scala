package org.atnos.eff

import org.atnos.eff.Eff.*

trait ListCreation {

  type _List[R] = List <= R
  type _list[R] = List |= R

  /** create a list effect with no values */
  def empty[R: _list, A]: Eff[R, A] =
    fromList(List())

  /** create a list effect from a single value */
  def singleton[R: _list, A](a: A): Eff[R, A] =
    fromList(List(a))

  /** create a list effect from a list of values */
  def values[R: _list, A](as: A*): Eff[R, A] =
    fromList(as.toList)

  /** create a list effect from a list of values */
  def fromList[R: _list, A](as: List[A]): Eff[R, A] =
    send[List, R, A](as)
}

object ListCreation extends ListCreation
