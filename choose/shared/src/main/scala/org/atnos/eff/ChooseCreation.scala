package org.atnos.eff

import Eff._

trait ChooseCreation {

  type _Choose[R] = Choose <= R
  type _choose[R] = Choose |= R

  def zero[R: _choose, A]: Eff[R, A] =
    send[Choose, R, A](ChooseZero[A]())

  def plus[R: _choose, A](a1: => Eff[R, A], a2: => Eff[R, A]): Eff[R, A] =
    EffMonad[R].flatMap(send(ChoosePlus))((b: Boolean) => if (b) a1 else a2)

  def chooseFrom[R: _choose, A](as: List[A]): Eff[R, A] =
    as match {
      case Nil => send[Choose, R, A](ChooseZero[A]())
      case a :: rest => plus(EffMonad[R].pure(a), chooseFrom(rest))
    }
}

object ChooseCreation extends ChooseCreation
