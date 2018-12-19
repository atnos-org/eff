package org.atnos.eff.syntax

import cats.data.{Ior, IorNel, NonEmptyList, ValidatedNel}
import org.atnos.eff._
import cats.{Applicative, Semigroup}

object validate extends validate

trait validate {

  implicit class ValidateEffectOps[R, A](e: Eff[R, A]) {

    def runNel[E](implicit m: Member[Validate[E, ?], R]): Eff[m.Out, NonEmptyList[E] Either A] =
      ValidateInterpretation.runNel(e)(m.aux)

    def runMap[E, L : Semigroup](map: E => L)(implicit m: Member[Validate[E, ?], R]): Eff[m.Out, L Either A] =
      ValidateInterpretation.runMap(e)(map)(Semigroup[L], m.aux)

    def runValidatedNel[E](implicit m: Member[Validate[E, ?], R]): Eff[m.Out, ValidatedNel[E, A]] =
      ValidateInterpretation.runValidatedNel(e)(m.aux)

    def runIorMap[E, L : Semigroup](map: E => L)(implicit m: Member[Validate[E, ?], R]): Eff[m.Out, L Ior A] =
      ValidateInterpretation.runIorMap(e)(map)(Semigroup[L], m.aux)

    def runIorNel[E](implicit m: Member[Validate[E, ?], R]): Eff[m.Out, E IorNel A] =
      ValidateInterpretation.runIorNel(e)(m.aux)

    @deprecated("Use catchFirstWrong or more general catchWrongs instead", "5.4.0")
    def catchWrong[E](handle: E => Eff[R, A])(implicit m: Member[Validate[E, ?], R]): Eff[R, A] =
      ValidateInterpretation.catchWrong(e)(handle)

    def catchWrongs[E, S[_]: Applicative](handle: S[E] => Eff[R, A])(implicit m: Member[Validate[E, ?], R], semi: Semigroup[S[E]]): Eff[R, A] =
      ValidateInterpretation.catchWrongs(e)(handle)

    def catchFirstWrong[E](handle: E => Eff[R, A])(implicit m: Member[Validate[E, ?], R]): Eff[R, A] =
      ValidateInterpretation.catchFirstWrong(e)(handle)

    def catchAllWrongs[E](handle: NonEmptyList[E] => Eff[R, A])(implicit m: Member[Validate[E, ?], R]): Eff[R, A] =
      ValidateInterpretation.catchAllWrongs(e)(handle)
  }

}
