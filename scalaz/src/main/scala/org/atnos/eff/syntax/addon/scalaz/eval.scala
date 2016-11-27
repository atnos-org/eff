package org.atnos.eff
package syntax.addon.scalaz

import cats.Eval

import scalaz._

object eval extends org.atnos.eff.syntax.eval with eval

trait eval {

  implicit def toEvalEffectScalazOps[R, A](e: Eff[R, A]): EvalEffectScalazOps[R, A] = new EvalEffectScalazOps[R, A](e)

}

final class EvalEffectScalazOps[R, A](val e: Eff[R, A]) extends AnyVal {

  def attemptEvalDisjunction[U](implicit member: Member.Aux[Eval, R, U]): Eff[U, Throwable \/ A] =
    addon.scalaz.eval.attemptEvalDisjunction(e)

}
