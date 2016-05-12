package org.atnos.eff.syntax

import cats.data.Xor
import org.atnos.eff._

object xor extends xor

trait xor {

  implicit class XorEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runXor[E](implicit member: Member[(E Xor ?), R]): Eff[member.Out, E Xor A] =
      XorInterpretation.runXor(e)(member.aux)

    def runEither[E, U <: Effects](implicit member: Member[(E Xor ?), R]): Eff[member.Out, E Either A] =
      XorInterpretation.runEither(e)(member.aux)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
      XorInterpretation.catchLeft(e)(handle)(member)
  }

}
