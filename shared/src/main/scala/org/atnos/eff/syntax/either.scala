package org.atnos.eff.syntax

import cats.Semigroup
import org.atnos.eff._

object either extends either

trait either {

  implicit class EitherEffectOps[R, A](e: Eff[R, A]) {

    def runEither[E, U](implicit m: Member.Aux[(E Either ?), R, U]): Eff[U, E Either A] =
      EitherInterpretation.runEither(e)(m)

    def runEitherCombine[E, U](implicit m: Member.Aux[(E Either ?), R, U], s: Semigroup[E]): Eff[U, E Either A] =
      EitherInterpretation.runEitherCombine(e)(m, s)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E Either ?), R]): Eff[R, A] =
      EitherInterpretation.catchLeft(e)(handle)(member)

    def catchLeftCombine[E](handle: E => Eff[R, A])(implicit member: Member[(E Either ?), R], s: Semigroup[E]): Eff[R, A] =
      EitherInterpretation.catchLeftCombine(e)(handle)(member, s)

    def localEither[BR, U, C, B](getter: C => B)(implicit m1: Member.Aux[C Either ?, R, U], m2: Member.Aux[B Either  ?, BR, U]): Eff[BR, A] =
      EitherInterpretation.localEither[R, BR, U, C, B, A](e, getter)

    def runLocalEither[U, C, B](getter: C => B)(implicit sr: Member.Aux[C Either ?, R, U], br: (B Either ?) |= U): Eff[U, A] =
      EitherInterpretation.runLocalEither[R, U, C, B, A](e, getter)

  }

}
