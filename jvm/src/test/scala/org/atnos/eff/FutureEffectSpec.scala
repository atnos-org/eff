package org.atnos.eff

import org.specs2.Specification
import org.atnos.eff.syntax.all._
import all._

import scala.concurrent._
import duration._
import cats.Eval
import org.specs2.concurrent.ExecutionEnv
import cats.implicits._
import cats.data.Writer
import scala.collection.mutable.ListBuffer

class FutureEffectSpec(implicit ee: ExecutionEnv) extends Specification { def is = s2"""

 A future effect can be added to a stack of effects $e1
 A future execution can be delayed $e2

 A Future value execution can be delayed $e3

 A Future can be lifted to a stack of effects $e4

 We can use a partial function to recover from an exception $e5

 We can attempt a future and use an Either effect for the exception $e6

"""

  type S = Fx.fx2[Future, Eval]

  def e1 = {
    def action[R :_future :_option]: Eff[R, Int] = for {
      a <- async(10)
      b <- option.some(a)
    } yield a + b

    action[Fx.fx2[Future, Option]].runOption.awaitFuture(1.second).run ==== Right(Some(20))
  }

  def e2 = {
    def action[R :_future :_eval]: Eff[R, Int] =
      delay(10).flatMap(v => async(v))

    action[S].runEval.awaitFuture(1.second).run ==== Right(10)
  }

  def e3 = {
    def action[R :_future :_eval]: Eff[R, Int] =
      delay(Future(10)).flatMap(v => send(v))

    action[S].runEval.awaitFuture(1.second).run ==== Right(10)
  }

  def e4 = {

    def future: Future[Int] = Future(10)
    def action[R :_future :_eval]: Eff[R, Int] = future.liftFuture

    action[S].runEval.awaitFuture(1.second).run ==== Right(10)
  }

  def e5 = {
    type WriterString[A] = Writer[String, A]
    type _log[R] = WriterString |= R

    def action[R :_future :_log] =
      tell("message") >>
      async { throw new TimeoutException; 1 }

    type S1 = Fx.fx2[WriterString, Future]

    val messages = new ListBuffer[String]
    val action1 = send(action[S1].runWriterUnsafe((s: String) => messages.append(s)).detach.recover { case e: TimeoutException => 2 })

    (action1.detach must be_==(2).await) and
     (messages.toList === List("message"))

  }

  def e6 = {
    def action[R :_future :_eval :_throwableEither] =
      FutureEffect.attemptFuture(Future { throw new TimeoutException; 1 })

    type S1 = Fx.fx3[ThrowableEither, Eval, Future]

    action[S1].runEither.runEval.detach must beLeft((e: Throwable) => e must haveClass[TimeoutException]).await
  }
}
