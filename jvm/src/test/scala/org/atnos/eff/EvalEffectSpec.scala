package org.atnos.eff

import org.specs2.Specification
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import cats.syntax.all._
import cats.instances.all._
import cats.instances.all._
import cats.data._, Xor._
import cats.Eval

class EvalEffectSpec extends Specification { def is = s2"""

 run is stack safe with Eval   $stacksafeRun

"""

  type E = Fx.fx1[Eval]

  val list = (1 to 5000).toList

  def stacksafeRun = {
    val action = list.traverseU(i => EvalEffect.delay(i))
    action.runEval.run ==== list
  }

  def stacksafeAttempt = {
    val action = list.traverseU(i => EvalEffect.delay(i))
    action.attemptEval.run ==== Right(list)
  }
}

