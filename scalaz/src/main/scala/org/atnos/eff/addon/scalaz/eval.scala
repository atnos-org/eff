package org.atnos.eff
package addon.scalaz

import cats.Eval

import scalaz._

object eval extends eval

trait eval {

  def attemptEvalDisjunction[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, Throwable \/ A] =
    org.atnos.eff.all.attemptEval(r).map(_.fold(\/.left, \/.right))

}
