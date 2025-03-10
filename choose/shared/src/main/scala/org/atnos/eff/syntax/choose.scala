package org.atnos.eff.syntax

import cats.Alternative
import org.atnos.eff.*

object choose extends choose

trait choose {

  given chooseExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {
      def runChoose[F[_]: Alternative](using member: Member[Choose, R]): Eff[member.Out, F[A]] =
        ChooseInterpretation.runChoose(e)(using Alternative[F], member.aux)
    }
  }

}
