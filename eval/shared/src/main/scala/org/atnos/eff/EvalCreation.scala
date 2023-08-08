package org.atnos.eff

import cats._
import Eff._

trait EvalCreation extends EvalTypes {
  def now[R: _eval, A](a: A): Eff[R, A] =
    pure(a)

  def delay[R: _eval, A](a: => A): Eff[R, A] =
    send(cats.Eval.later(a))

  def defer[R: _eval, A](eff: => Eval[Eff[R, A]]): Eff[R, A] = {
    send(cats.Eval.defer(eff)).flatten
  }
}
