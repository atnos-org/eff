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
    type S = Future |: Option |: NoEffect
    implicit val f: Member.Aux[Future, S, Option |: NoEffect] = Member.first
    implicit val o: Member.Aux[Option, S, Future |: NoEffect] = Member.successor

    val action: Eff[S, Int] = for {
      a <- async(10)
      b <- option.some(a)
    } yield a + b

    action.runOption.awaitFuture(1.second).run ==== Xor.right(Some(20))

  }

  def e2 = {
    type S = Future |: Eval |: NoEffect
    implicit val f: Member.Aux[Future, S, Eval |: NoEffect] = Member.first
    implicit val e: Member.Aux[Eval, S, Future |: NoEffect] = Member.successor

    val action: Eff[S, Int] =
      delay(10).flatMap(v => async[S, Int](v))

    action.runEval.awaitFuture(1.second).run ==== Xor.right(10)

  }

  def e3 = {
    type S = Future |: Eval |: NoEffect
    implicit val f: Member.Aux[Future, S, Eval |: NoEffect] = Member.first
    implicit val e: Member.Aux[Eval, S, Future |: NoEffect] = Member.successor

    val action: Eff[S, Int] =
      delay(Future(10)).flatMap(v => send[Future, S, Int](v))

    action.runEval.awaitFuture(1.second).run ==== Xor.right(10)

  }

  def e4 = {
    type S = Future |: Eval |: NoEffect

    def future: Future[Int] = Future(10)

    val action: Eff[S, Int] = future.liftFuture

    action.runEval.awaitFuture(1.second).run ==== Xor.right(10)
  }
}
