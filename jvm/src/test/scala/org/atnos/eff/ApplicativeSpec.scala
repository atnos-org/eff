package org.atnos.eff

import org.specs2.{ScalaCheck, Specification}
import org.atnos.eff.syntax.all._
import org.atnos.eff.all._
import scala.concurrent._
import duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.Xor
import cats.Eval
import cats.implicits._
import org.specs2.matcher.ThrownExpectations
import org.scalacheck.Gen.{listOfN, choose => chooseInt}
import org.specs2.concurrent.ExecutionEnv
import scala.collection.mutable.ListBuffer

class ApplicativeSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 It is possible to use an applicative instance to execute effects "in parallel"
  as a monad                         $asMonad
  as an applicative                  $asApplicative
  as a monad prop                    $asMonadProp
  as an applicative prop             $asApplicativeProp
  it is stacksafe with list.traverse $stacksafeList
  it is stacksafe with eff.traverse  $stacksafeEff

"""

  type S = Future |:: Eval

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
  }.setGen(chooseInt(5, 10).flatMap(listOfN(_, chooseInt(10, 50)))).
    set(minTestsOk = 20)

  def asApplicativeProp = prop { elements: List[Int] =>

    val messages = new ListBuffer[String]
    val actionApplicative: Eff[S, List[Int]] =
      elements.map(i => delay[S, Int](i).flatMap(v => async(register(v, messages)))).sequenceA

    Eff.detach(actionApplicative.runEval) must be_==(elements).await

    "messages are not received in the same order" ==> {
      messages.toList !=== elements.map("got "+_)
    }
  }.setGen(chooseInt(5, 10).flatMap(listOfN(_, chooseInt(10, 50)))).
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
