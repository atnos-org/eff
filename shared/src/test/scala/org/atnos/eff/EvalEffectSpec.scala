package org.atnos.eff

import org.specs2.Specification
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import cats.syntax.all._
import cats.Eval

class EvalEffectSpec extends Specification with Specs2Compat { def is = s2"""

 run is stack safe with Eval               $stacksafeRun
 attempt is stack safe with Eval           $stacksafeAttempt
 recursion in Eval.defer is stack safe     $stacksafeRecursion

"""

  type E = Fx.fx1[Eval]

  val list = (1 to 5000).toList

  def stacksafeRun = {
    val action = list.traverse(i => EvalEffect.delay(i))
    action.runEval.run ==== list
  }

  def stacksafeAttempt = {
    val action = list.traverse(i => EvalEffect.delay(i))
    action.attemptEval.run ==== Right(list)
  }

  def stacksafeRecursion = {
    def loop(i: Int): Eval[Eff[Fx.fx1[Eval], Int]] =
      if (i == 0) {
        Eval.now(Eff.pure(1))
      } else {
        Eval.now(org.atnos.eff.eval.defer(loop(i - 1)).map(_  + 1))
      }

    loop(100000).value.runEval.run ==== 100001
  }
}

