package org.atnos.eff

import org.specs2.Specification
import org.atnos.eff.syntax.all._
import all._

import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.Xor
import cats.Eval

class FutureEffectSpec extends Specification { def is = s2"""

 A future effect can be added to a stack of effects $e1
 A future execution can be delayed $e2

 A Future value execution can be delayed $e3

 A Future can be lifted to a stack of effects $e4

"""

  def e1 = {
    def action[R :_future :_option]: Eff[R, Int] = for {
      a <- async(10)
      b <- option.some(a)
    } yield a + b

    type S = Future |: Option |: NoEffect

    action[S].runOption.awaitFuture(1.second).run ==== Xor.right(Some(20))
  }

  def e2 = {
    def action[R :_future :_eval]: Eff[R, Int] =
      delay(10).flatMap(v => async(v))

    type S = Future |: Eval |: NoEffect

    action[S].runEval.awaitFuture(1.second).run ==== Xor.right(10)

  }

  def e3 = {
    def action[R :_future :_eval]: Eff[R, Int] =
      delay(Future(10)).flatMap(v => send(v))

    type S = Future |: Eval |: NoEffect

    action[S].runEval.awaitFuture(1.second).run ==== Xor.right(10)
  }

  def e4 = {

    def future: Future[Int] = Future(10)
    def action[R :_future :_eval]: Eff[R, Int] = future.liftFuture

    type S = Future |: Eval |: NoEffect

    action[S].runEval.awaitFuture(1.second).run ==== Xor.right(10)
  }
}
