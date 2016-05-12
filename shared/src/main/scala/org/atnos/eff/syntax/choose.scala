package org.atnos.eff.syntax

import cats.Alternative
import org.atnos.eff._

object choose extends choose

trait choose {

  implicit class ChooseEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runChoose[F[_] : Alternative](implicit member: Member[Choose, R]): Eff[member.Out, F[A]] =
      ChooseInterpretation.runChoose(e)(Alternative[F], member.aux)

  }

}
