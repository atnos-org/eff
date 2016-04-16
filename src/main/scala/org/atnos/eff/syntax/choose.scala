package org.atnos.eff.syntax

import cats.Alternative
import org.atnos.eff._

object choose extends choose

trait choose {

  implicit class ChooseEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runChoose[U <: Effects, F[_] : Alternative](implicit member: Member.Aux[Choose, R, U]): Eff[U, F[A]] =
      ChooseInterpretation.runChoose(e)

  }

}
