package org.atnos.eff.syntax

import cats.data.Reader
import org.atnos.eff._

object reader extends reader

trait reader {

  implicit class ReaderEffectOps[R, A](e: Eff[R, A]) {

    def runReader[C](c: C)(implicit member: Member[Reader[C, ?], R]): Eff[member.Out, A] =
      ReaderInterpretation.runReader(c)(e)(member.aux)

    def localReader[BR, U, C, B](getter: B => C)(implicit m1: Member.Aux[Reader[C, ?], R, U], m2: Member.Aux[Reader[B,  ?], BR, U]): Eff[BR, A] =
      ReaderInterpretation.localReader[R, BR, U, C, B, A](e, getter)
  }

}

