package org.atnos.eff
package addon.scalaz

import cats.Eval
import scalaz.*

object eval extends eval

trait eval {

  def attemptEvalDisjunction[R, U, A](r: Eff[R, A])(using Member.Aux[Eval, R, U]): Eff[U, Throwable \/ A] =
    org.atnos.eff.eval.attemptEval(r).map(_.fold(\/.left, \/.right))

}
