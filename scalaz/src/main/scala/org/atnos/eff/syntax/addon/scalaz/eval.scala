package org.atnos.eff
package syntax.addon.scalaz

import cats.Eval
import scalaz.*

object eval extends org.atnos.eff.syntax.eval with eval

trait eval {

  given scalazEvalExtension: AnyRef with {

    extension [R, A](e: Eff[R, A]) {
      def attemptEvalDisjunction[U](using Member.Aux[Eval, R, U]): Eff[U, Throwable \/ A] =
        addon.scalaz.eval.attemptEvalDisjunction(e)
    }
  }

}
