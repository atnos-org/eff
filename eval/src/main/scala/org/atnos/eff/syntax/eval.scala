package org.atnos.eff.syntax

import cats.*
import org.atnos.eff.*

object eval extends eval

trait eval {

  given evalExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {
      def runEval(using member: Member[Eval, R]): Eff[member.Out, A] =
        EvalInterpretation.runEval(e)(using member.aux)

      def attemptEval(using member: Member[Eval, R]): Eff[member.Out, Either[Throwable, A]] =
        EvalInterpretation.attemptEval(e)(using member.aux)
    }
  }

}
