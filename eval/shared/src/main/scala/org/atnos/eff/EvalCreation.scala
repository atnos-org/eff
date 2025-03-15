package org.atnos.eff

import cats.*
import org.atnos.eff.Eff.pure
import org.atnos.eff.Eff.send

trait EvalCreation extends EvalTypes {
  def now[R: _eval, A](a: A): Eff[R, A] =
    pure(a)

  def delay[R: _eval, A](a: => A): Eff[R, A] =
    send(cats.Eval.later(a))

  def defer[R: _eval, A](eff: => Eval[Eff[R, A]]): Eff[R, A] = {
    send(cats.Eval.defer(eff)).flatten
  }
}
