package org.atnos.eff.syntax

import cats.data.Xor
import org.atnos.eff.{DisjunctionInterpretation, Eff, Effects, Member}

object disjunction {
  implicit class DisjunctionEffectOps[R <: Effects, A](e: Eff[R, A]) {
    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
      DisjunctionInterpretation.catchLeft(e)(handle)(member)
  }

}
