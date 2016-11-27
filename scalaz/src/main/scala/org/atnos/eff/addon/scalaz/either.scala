package org.atnos.eff
package addon.scalaz

import scalaz._, Scalaz._

object either extends either

trait either {

  def fromDisjunction[R, E, A](ea: E \/ A)(implicit member: (E Either ?) |= R): Eff[R, A] =
    all.fromEither(ea.fold(Left.apply, Right.apply))

  def runDisjunction[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E Either ?), R, U]): Eff[U, E \/ A] =
    all.runEither(r).map(_.fold(\/.left, \/.right))

  /** run the Either effect, yielding E Either A and combine all Es */
  def runDisjunctionCombine[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E Either ?), R, U], s: Semigroup[E]): Eff[U, E \/ A] =
    all.runEitherCombine(r)(m, catsSemigroup(s)).map(_.fold(\/.left, \/.right))

  /** catch and handle a possible left value. The value is the combination of all failures in case of an applicative */
  def catchLeftCombine[R, E, A](r: Eff[R, A])(handle: E => Eff[R, A])(implicit member: (E Either ?) /= R, s: Semigroup[E]): Eff[R, A] =
    all.catchLeftCombine(r)(handle)(member, catsSemigroup(s))

}
