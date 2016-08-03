package org.atnos.eff

import org.specs2.Specification
import org.atnos.eff.syntax.all._
import all._

import scala.concurrent._
import duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.Xor
import cats.Eval
import org.specs2.concurrent.ExecutionEnv
import cats.implicits._
import cats.data.Writer
import scala.collection.mutable.ListBuffer
import org.specs2.matcher.XorMatchers._

class FutureEffectSpec(implicit ee: ExecutionEnv) extends Specification { def is = s2"""

 A future effect can be added to a stack of effects $e1
 A future execution can be delayed $e2

 A Future value execution can be delayed $e3

 A Future can be lifted to a stack of effects $e4

 We can use a partial function to recover from an exception $e5

 We can attempt a future and use an Xor effect for the exception $e6

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

  def e5 = {
    type WriterString[A] = Writer[String, A]
    type _log[R] = WriterString |= R

    def action[R :_future :_log] =
      tell("message") >>
      async { throw new TimeoutException; 1 }

    type S = WriterString |: Future |: NoEffect

    val messages = new ListBuffer[String]
    val action1 = send(action[S].runWriterUnsafe((s: String) => messages.append(s)).detach.recover { case e: TimeoutException => 2 })

    (action1.detach must be_==(2).await) and
     (messages.toList === List("message"))

  }

  def e6 = {
    def action[R :_future :_eval :_throwableXor] =
      FutureEffect.attemptFuture(Future { throw new TimeoutException; 1 })

    type S = ThrowableXor |: Eval |: Future |: NoEffect
    val s = Fx[S]
    action[s.Fx].runXor.runEval.detach must beXorLeft((e: Throwable) => e must haveClass[TimeoutException]).await
  }
}
