package org.atnos.eff
package addon.scalaz

import scalaz.*

object either extends either

trait either {

  def fromDisjunction[R, E, A](ea: E \/ A)(implicit member: Either[E, *] |= R): Eff[R, A] =
    org.atnos.eff.either.fromEither(ea.toEither)

  def runDisjunction[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[Either[E, *], R, U]): Eff[U, E \/ A] =
    org.atnos.eff.either.runEither(r).map(_.fold(\/.left, \/.right))

  /** run the Either effect, yielding E Either A and combine all Es */
  def runDisjunctionCombine[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[Either[E, *], R, U], s: Semigroup[E]): Eff[U, E \/ A] =
    org.atnos.eff.either.runEitherCombine(r)(using m, catsSemigroup(s)).map(_.fold(\/.left, \/.right))

  /** catch and handle a possible left value. The value is the combination of all failures in case of an applicative */
  def catchLeftCombine[R, E, A](r: Eff[R, A])(handle: E => Eff[R, A])(implicit member: Either[E, *] /= R, s: Semigroup[E]): Eff[R, A] =
    org.atnos.eff.either.catchLeftCombine(r)(handle)(using member, catsSemigroup(s))

}
