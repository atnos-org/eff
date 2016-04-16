package org.atnos.eff.syntax

import cats.data.Xor
import org.atnos.eff._

object disjunction extends disjunction

trait disjunction {

  implicit class DisjunctionEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runXor[E, U <: Effects](implicit member: Member.Aux[(E Xor ?), R, U]): Eff[U, E Xor A] =
      DisjunctionInterpretation.runXor(e)

    def runEither[E, U <: Effects](implicit member: Member.Aux[(E Xor ?), R, U]): Eff[U, E Either A] =
      DisjunctionInterpretation.runEither(e)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
      DisjunctionInterpretation.catchLeft(e)(handle)(member)
  }

}
