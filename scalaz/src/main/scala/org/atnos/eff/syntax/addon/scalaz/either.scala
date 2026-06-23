package org.atnos.eff
package syntax.addon.scalaz

import scalaz.*

object either extends org.atnos.eff.syntax.either with either

trait either {
  given scalazEitherExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {
      def runDisjunction[U, E](using Member.Aux[Either[E, *], R, U]): Eff[U, E \/ A] =
        addon.scalaz.either.runDisjunction(e)

      def runDisjunctionCombine[U, E](using Member.Aux[Either[E, *], R, U], Semigroup[E]): Eff[U, E \/ A] =
        addon.scalaz.either.runDisjunctionCombine(e)

      def catchLeftCombine[E](handle: E => Eff[R, A])(using Either[E, *] /= R, Semigroup[E]): Eff[R, A] =
        addon.scalaz.either.catchLeftCombine(e)(handle)
    }
  }
}
