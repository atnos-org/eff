package org.atnos.eff
package syntax.addon.scalaz

import scalaz._

object validate extends org.atnos.eff.syntax.validate with validate

trait validate {

  implicit def toValidateEffectScalazOps[R, A](e: Eff[R, A]): ValidateEffectScalazOps[R, A] =
    new ValidateEffectScalazOps[R, A](e)

}

final class ValidateEffectScalazOps[R, A](private val e: Eff[R, A]) extends AnyVal {

  def runValidationNel[U, E](r: Eff[R, A])(implicit m: Member.Aux[Validate[E, *], R, U]): Eff[U, ValidationNel[E, A]] =
    addon.scalaz.validate.runValidationNel(e)

  def runNelDisjunction[U, E](r: Eff[R, A])(implicit m: Member.Aux[Validate[E, *], R, U]): Eff[U, NonEmptyList[E] \/ A] =
    addon.scalaz.validate.runNelDisjunction(e)

  def runMapDisjunction[U, E, L : Semigroup](map: E => L)(implicit m: Member.Aux[Validate[E, *], R, U]): Eff[U, L \/ A] =
    addon.scalaz.validate.runMapDisjunction(e)(map)

}
