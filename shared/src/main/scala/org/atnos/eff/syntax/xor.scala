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

    def localXor[BR, U, C, B](getter: C => B)(implicit m1: Member.Aux[C Xor ?, R, U], m2: Member.Aux[B Xor  ?, BR, U]): Eff[BR, A] =
      XorInterpretation.localXor[R, BR, U, C, B, A](e, getter)

  }

}
