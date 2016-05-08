package org.atnos.eff

import org.specs2.Specification
import org.atnos.eff.syntax.all._
import org.atnos.eff.all._

import scala.concurrent._
import duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.Xor
import cats.std.list.listInstance
import cats.syntax.all._
import org.specs2.matcher.ThrownExpectations

import scala.collection.mutable.ListBuffer

class ApplicativeSpec extends Specification with ThrownExpectations { def is = s2"""

 It is possible to use an applicative instance to execute effects "in parallel"
  as a monad $asMonad
  as an applicative $asApplicative
  it is stacksafe $stacksafe

"""

  type S = Future |: Eval |: NoEffect
  implicit val f: Member.Aux[Future, S, Eval |: NoEffect] = Member.first
  implicit val e: Member.Aux[Eval, S, Future |: NoEffect] = Member.successor

  def asMonad = {

    val messages = new ListBuffer[String]

    val actionMonadic: Eff[S, List[Int]] =
      elements.map(i => delay[S, Int](i).flatMap(v => async(register(v, messages)))).sequence

    actionMonadic.runEval.awaitFuture(1.second).run ==== Xor.right(elements)
    messages.toList ==== elements.map("got "+_)
  }

  def asApplicative = {

    val messages = new ListBuffer[String]

    val actionApplicative: Eff[S, List[Int]] =
      Eff.sequence(elements.map(i => delay[S, Int](i).flatMap(v => async(register(v, messages)))))

    actionApplicative.runEval.awaitFuture(1.second).run ==== Xor.right(elements)
    messages.toList ==== elements.reverse.map("got "+_)
  }

  def stacksafe = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => FutureEffect.async[S, String](i.toString))

    action.awaitFuture(1.second).runEval.run ==== list.map(_.toString).right

  }

  /**
   * HELPERS
   */

  def register(i: Int, messages: ListBuffer[String]) = {
    Thread.sleep(i.toLong)
    messages.append("got "+i)
    i
  }

  val elements = List(500, 300, 100)
}
