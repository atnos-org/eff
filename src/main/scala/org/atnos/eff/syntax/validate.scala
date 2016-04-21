package org.atnos.eff.syntax

import cats.data.{NonEmptyList, ValidatedNel, Xor}
import org.atnos.eff._
import cats.Semigroup

object validate extends validate

trait validate {

  implicit class ValidateEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runNel[E, U <: Effects](implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, NonEmptyList[E] Xor A] =
      ValidateInterpretation.runNel(e)

    def runMap[E, U <: Effects, L : Semigroup](map: E => L)(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, L Xor A] =
      ValidateInterpretation.runMap(e)(map)

    def runValidatedNel[E, U <: Effects](implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, ValidatedNel[E, A]] =
      ValidateInterpretation.runValidatedNel(e)
  }

}
