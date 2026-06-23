package org.atnos.eff
package syntax.addon.scalaz

import scalaz.*

object validate extends org.atnos.eff.syntax.validate with validate

trait validate {

  given scalazValidateExtension: AnyRef with {

    extension [R, A](e: Eff[R, A]) {
      def runValidationNel[U, E](using Member.Aux[Validate[E, *], R, U]): Eff[U, ValidationNel[E, A]] =
        addon.scalaz.validate.runValidationNel(e)

      def runNelDisjunction[U, E](using Member.Aux[Validate[E, *], R, U]): Eff[U, NonEmptyList[E] \/ A] =
        addon.scalaz.validate.runNelDisjunction(e)

      def runMapDisjunction[U, E, L: Semigroup](map: E => L)(using Member.Aux[Validate[E, *], R, U]): Eff[U, L \/ A] =
        addon.scalaz.validate.runMapDisjunction(e)(map)
    }
  }

}
