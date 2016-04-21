package org.atnos.eff.syntax

import cats.data.Xor
import org.atnos.eff._

object either extends either

trait either {

  implicit class EitherEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runXor[E, U <: Effects](implicit member: Member.Aux[(E Xor ?), R, U]): Eff[U, E Xor A] =
      EitherInterpretation.runXor(e)

    def runEither[E, U <: Effects](implicit member: Member.Aux[(E Xor ?), R, U]): Eff[U, E Either A] =
      EitherInterpretation.runEither(e)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
      EitherInterpretation.catchLeft(e)(handle)(member)
  }

}
