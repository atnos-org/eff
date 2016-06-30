package org.atnos.eff.syntax

import cats._, data._
import org.atnos.eff._

object eval extends eval

trait eval {

  implicit class EvalEffectOps[R, A](e: Eff[R, A]) {

    def runEval(implicit member: Member[Eval, R]): Eff[member.Out, A] =
      EvalInterpretation.runEval(e)(member.aux)

    def attemptEval(implicit member: Member[Eval, R]): Eff[member.Out, Throwable Xor A] =
      EvalInterpretation.attemptEval(e)(member.aux)

  }

}

