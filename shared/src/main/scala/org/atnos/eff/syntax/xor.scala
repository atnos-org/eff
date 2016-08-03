package org.atnos.eff.syntax

import cats.data.Xor
import org.atnos.eff._

object xor extends xor

trait xor {

  implicit class XorEffectOps[R, A](e: Eff[R, A]) {

    def runXor[E, U](implicit m: Member.Aux[(E Xor ?), R, U]): Eff[U, E Xor A] =
      XorInterpretation.runXor(e)(m)

    def runEither[E, U](implicit m: Member.Aux[(E Xor ?), R, U]): Eff[U, E Either A] =
      XorInterpretation.runEither(e)(m)

    def catchLeft[E](handle: E => Eff[R, A])(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
      XorInterpretation.catchLeft(e)(handle)(member)

    def localXor[BR, U, C, B](getter: C => B)(implicit m1: Member.Aux[C Xor ?, R, U], m2: Member.Aux[B Xor  ?, BR, U]): Eff[BR, A] =
      XorInterpretation.localXor[R, BR, U, C, B, A](e, getter)

    def runLocalXor[U, C, B](getter: C => B)(implicit sr: Member.Aux[C Xor ?, R, U], br: (B Xor ?) |= U): Eff[U, A] =
      XorInterpretation.runLocalXor[R, U, C, B, A](e, getter)

  }

}
