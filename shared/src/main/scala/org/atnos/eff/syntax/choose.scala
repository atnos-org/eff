package org.atnos.eff.syntax

import cats.Alternative
import org.atnos.eff._

object choose extends choose

trait choose {
  implicit final def toChooseEffectOps[R, A](e: Eff[R, A]): ChooseEffectOps[R, A] = new ChooseEffectOps(e)
}

final class ChooseEffectOps[R, A](private val e: Eff[R, A]) extends AnyVal {

  def runChoose[F[_] : Alternative](implicit member: Member[Choose, R]): Eff[member.Out, F[A]] =
    ChooseInterpretation.runChoose(e)(Alternative[F], member.aux)

}
