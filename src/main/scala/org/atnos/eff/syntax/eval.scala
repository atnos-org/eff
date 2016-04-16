package org.atnos.eff.syntax

import cats._, data._
import org.atnos.eff._

object eval extends eval

trait eval {

  implicit class EvalEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runEval[U <: Effects](implicit member: Member.Aux[Eval, R, U]): Eff[U, A] =
      EvalInterpretation.runEval(e)

    def attemptEval[U <: Effects](implicit member: Member.Aux[Eval, R, U]): Eff[U, Throwable Xor A] =
      EvalInterpretation.attemptEval(e)

  }

}

