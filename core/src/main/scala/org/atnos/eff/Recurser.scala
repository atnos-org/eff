package org.atnos.eff

import cats.*

/**
 * Helper trait for computations
 * which might produce several M[X] in a stack of effects.
 *
 * Either we can produce an X to pass to a continuation or we're done
 *
 * For the applicative case we expect to be able to traverse a list
 * of effects and return an effect of a list of results OR
 * completely consume the effect and return a pure list of values
 */
trait Recurser[M[_], R, A, B] {
  def onPure(a: A): B
  def onEffect[X](m: M[X]): Either[X, Eff[R, B]]
  def onApplicative[X, T[_]: Traverse](ms: T[M[X]]): Either[T[X], M[T[X]]]
}
