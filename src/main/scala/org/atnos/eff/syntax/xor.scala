package org.atnos.eff.syntax

import cats.data.Xor
import org.atnos.eff._

object xor extends xor

trait xor {

  implicit class XorEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runXor[E, U <: Effects](implicit member: Member.Aux[(E Xor ?), R, U]): Eff[U, E Xor A] =
      XorInterpretation.runXor(e)

    def runEither[E, U <: Effects](implicit member: Member.Aux[(E Xor ?), R, U]): Eff[U, E Either A] =
      XorInterpretation.runEither(e)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
      XorInterpretation.catchLeft(e)(handle)(member)
  }

}
