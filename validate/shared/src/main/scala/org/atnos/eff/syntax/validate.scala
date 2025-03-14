package org.atnos.eff.syntax

import cats.Applicative
import cats.Semigroup
import cats.data.Ior
import cats.data.IorNel
import cats.data.NonEmptyList
import cats.data.ValidatedNel
import org.atnos.eff.*

object validate extends validate

trait validate {

  implicit class ValidateEffectOps[R, A](e: Eff[R, A]) {

    def runNel[E](implicit m: Member[Validate[E, *], R]): Eff[m.Out, Either[NonEmptyList[E], A]] =
      ValidateInterpretation.runNel(e)(using m.aux)

    def runMap[E, L: Semigroup](map: E => L)(implicit m: Member[Validate[E, *], R]): Eff[m.Out, Either[L, A]] =
      ValidateInterpretation.runMap(e)(map)(using Semigroup[L], m.aux)

    def runValidatedNel[E](implicit m: Member[Validate[E, *], R]): Eff[m.Out, ValidatedNel[E, A]] =
      ValidateInterpretation.runValidatedNel(e)(using m.aux)

    def runIorMap[E, L: Semigroup](map: E => L)(implicit m: Member[Validate[E, *], R]): Eff[m.Out, Ior[L, A]] =
      ValidateInterpretation.runIorMap(e)(map)(using Semigroup[L], m.aux)

    def runIorNel[E](implicit m: Member[Validate[E, *], R]): Eff[m.Out, IorNel[E, A]] =
      ValidateInterpretation.runIorNel(e)(using m.aux)

    def catchWrongs[E, S[_]: Applicative](handle: S[E] => Eff[R, A])(implicit m: Member[Validate[E, *], R], semi: Semigroup[S[E]]): Eff[R, A] =
      ValidateInterpretation.catchWrongs(e)(handle)

    def catchFirstWrong[E](handle: E => Eff[R, A])(implicit m: Member[Validate[E, *], R]): Eff[R, A] =
      ValidateInterpretation.catchFirstWrong(e)(handle)

    def catchLastWrong[E](handle: E => Eff[R, A])(implicit m: Member[Validate[E, *], R]): Eff[R, A] =
      ValidateInterpretation.catchLastWrong(e)(handle)

    def catchAllWrongs[E](handle: NonEmptyList[E] => Eff[R, A])(implicit m: Member[Validate[E, *], R]): Eff[R, A] =
      ValidateInterpretation.catchAllWrongs(e)(handle)
  }

}
