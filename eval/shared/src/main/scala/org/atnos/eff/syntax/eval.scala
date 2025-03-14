package org.atnos.eff.syntax

import cats.*
import org.atnos.eff.*

object eval extends eval

trait eval {

  implicit def toEvalEffectOps[R, A](e: Eff[R, A]): EvalEffectOps[R, A] = new EvalEffectOps[R, A](e)
}

final class EvalEffectOps[R, A](private val e: Eff[R, A]) extends AnyVal {

  def runEval(implicit member: Member[Eval, R]): Eff[member.Out, A] =
    EvalInterpretation.runEval(e)(using member.aux)

  def attemptEval(implicit member: Member[Eval, R]): Eff[member.Out, Throwable Either A] =
    EvalInterpretation.attemptEval(e)(using member.aux)

}
