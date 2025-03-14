package org.atnos.eff
package syntax.addon.scalaz

import scalaz.*

object either extends org.atnos.eff.syntax.either with either

trait either {
  implicit final def toEitherEffectScalazOps[R, A](e: Eff[R, A]): EitherEffectScalazOps[R, A] = new EitherEffectScalazOps[R, A](e)
}

final class EitherEffectScalazOps[R, A](private val e: Eff[R, A]) extends AnyVal {
  def runDisjunction[U, E](implicit m: Member.Aux[Either[E, *], R, U]): Eff[U, E \/ A] =
    addon.scalaz.either.runDisjunction(e)

  def runDisjunctionCombine[U, E](implicit m: Member.Aux[Either[E, *], R, U], s: Semigroup[E]): Eff[U, E \/ A] =
    addon.scalaz.either.runDisjunctionCombine(e)

  def catchLeftCombine[E](handle: E => Eff[R, A])(implicit member: Either[E, *] /= R, s: Semigroup[E]): Eff[R, A] =
    addon.scalaz.either.catchLeftCombine(e)(handle)

}
