package org.specs2.control.eff

import org.specs2.Specification
import Eff._
import Effects._
import EvalEffect._

import cats.syntax.all._
import cats.std.all._
import cats.std.all._
import cats.data._, Xor._

class EvalEffectSpec extends Specification { def is = s2"""

 run is stack safe with Eval   $stacksafeRun

"""

  type E = Eval |: NoEffect

  val list = (1 to 5000).toList

  def stacksafeRun = {
    val action = list.traverseU(i => EvalEffect.delay(i))
    run(runEval(action)) ==== list
  }

  def stacksafeAttempt = {
    val action = list.traverseU(i => EvalEffect.delay(i))
    run(attemptEval(action)) ==== Right(list)
  }
}

