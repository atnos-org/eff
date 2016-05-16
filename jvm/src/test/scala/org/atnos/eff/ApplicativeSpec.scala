package org.atnos.eff

import org.specs2.{ScalaCheck, Specification}
import org.atnos.eff.syntax.all._
import org.atnos.eff.all._

import scala.concurrent._
import duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.Xor
import cats.std.list.listInstance
import cats.syntax.all._
import org.specs2.matcher.ThrownExpectations
import org.scalacheck.Gen.{choose => chooseInt, listOfN}

import scala.collection.mutable.ListBuffer

class ApplicativeSpec extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 It is possible to use an applicative instance to execute effects "in parallel"
  as a monad $asMonad
  as an applicative $asApplicative
  as a monad prop $asMonadProp
  as an applicative prop $asApplicativeProp
  it is stacksafe with list.traverse $stacksafeList
  it is stacksafe with eff.traverse  $stacksafeEff

"""

  type S = Future |: Eval |: NoEffect
  implicit val f: Member.Aux[Future, S, Eval |: NoEffect] = Member.first
  implicit val e: Member.Aux[Eval, S, Future |: NoEffect] = Member.successor


  val elements = List(1000, 500, 300, 100, 50)

  def asMonad = {

    val messages = new ListBuffer[String]

    val actionMonadic: Eff[S, List[Int]] =
      elements.map(i => delay[S, Int](i).flatMap(v => async(register(v, messages)))).sequence

    actionMonadic.runEval.awaitFuture(2.seconds).run ==== Xor.right(elements)
    messages.toList ==== elements.map("got "+_)
  }

  def asApplicative = {

    val messages = new ListBuffer[String]

    val actionApplicative: Eff[S, List[Int]] =
      elements.map(i => delay[S, Int](i).flatMap(v => async(register(v, messages)))).sequenceA

    actionApplicative.runEval.awaitFuture(2.seconds).run ==== Xor.right(elements)

    "messages are received in the reverse order, starting with the fastest one" ==> {
      messages.toList ==== elements.reverse.map("got "+_)
    }
  }

  def asMonadProp = prop { elements: List[Int] =>

    val messages = new ListBuffer[String]
    val actionMonadic: Eff[S, List[Int]] =
      elements.map(i => delay[S, Int](i).flatMap(v => async(register(v, messages)))).sequence

    actionMonadic.runEval.awaitFuture(2.seconds).run ==== Xor.right(elements)

    "messages are received in the same order" ==> {
      messages.toList ==== elements.map("got "+_)
    }
  }.setGen(chooseInt(2, 10).flatMap(listOfN(_, chooseInt(10, 500)))).
    set(minTestsOk = 20)

  def asApplicativeProp = prop { elements: List[Int] =>

    val messages = new ListBuffer[String]
    val actionApplicative: Eff[S, List[Int]] =
      elements.map(i => delay[S, Int](i).flatMap(v => async(register(v, messages)))).sequenceA

    actionApplicative.runEval.awaitFuture(2.seconds).run ==== Xor.right(elements)

    "messages are not received in the same order" ==> {
      messages.toList !=== elements.map("got "+_)
    }
  }.setGen(chooseInt(2, 10).flatMap(listOfN(_, chooseInt(10, 500)))).
    set(minTestsOk = 20)

  def stacksafeList = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => FutureEffect.async[S, String](i.toString))

    action.awaitFuture(1.second).runEval.run ==== list.map(_.toString).right
  }

  def stacksafeEff = {
    val list = (1 to 5000).toList
    val action = list.traverseA(i => FutureEffect.async[S, String](i.toString))

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
}
