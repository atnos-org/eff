package org.atnos.eff.monix

import org.atnos.eff._
import org.atnos.eff.eff._
import org.atnos.eff.eval._
import org.atnos.eff.option._
import org.atnos.eff.monix._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.monix._

import scala.concurrent._, duration._
import cats.data.Xor
import cats.Eval
import cats.implicits._
import _root_.monix.execution.Scheduler.Implicits.global
import _root_.monix.eval._
import org.specs2._
import org.scalacheck.Gen.{choose => chooseInt, listOfN}
import scala.collection.mutable.ListBuffer
import TaskEffect._

class TaskEffectSpec extends Specification with ScalaCheck { def is = s2"""

 A Task effect can be added to a stack of effects $e1
 A Task execution can be delayed $e2

 A Task value execution can be delayed $e3

 Tasks can be executed concurrently $e4
 Tasks can be executed sequentially $e5

"""

  def e1 = {
    type S = Task |:: Option

    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- async(10)
      b <- option.some(a)
    } yield a + b

    action[S].runOption.awaitTask(1.second).run ==== Xor.right(Some(20))

  }

  def e2 = {
    type S = Task |:: Eval

    def action[R :_task :_eval]: Eff[R, Int] =
      delay(10).flatMap(v => async(v))

    action[S].runEval.awaitTask(1.second).run ==== Xor.right(10)

  }

  def e3 = {
    type S = Task |:: Eval

    def action[R :_task :_eval]: Eff[R, Int] =
      delay(Task(10)).flatMap(v => send(v))

    action[S].runEval.awaitTask(1.second).run ==== Xor.right(10)

  }

  def e4 = prop { elements: List[Int] =>
    type S = Task |: NoEffect

    val messages = new ListBuffer[String]
    val actionApplicative: Eff[S, List[Int]] =
      elements.map(i => async(register(i, messages))).sequenceA

    actionApplicative.awaitTask(2.seconds).run ==== Xor.right(elements)

    "messages are not received in the same order" ==> {
      messages.toList !=== elements.map("got "+_)
    }
  }.setGen(chooseInt(5, 10).flatMap(listOfN(_, chooseInt(10, 50)))).
    set(minTestsOk = 20, workers = 5)

  def e5 = prop { elements: List[Int] =>
    type S = Task |: NoEffect

    val messages = new ListBuffer[String]
    val actionApplicative: Eff[S, List[Int]] =
      elements.map(i => async(register(i, messages))).sequence

    actionApplicative.awaitTask(2.seconds).run ==== Xor.right(elements)

    "messages are  received in the same order" ==> {
      messages.toList === elements.map("got "+_)
    }
  }.setGen(chooseInt(5, 10).flatMap(listOfN(_, chooseInt(10, 50)))).
    set(minTestsOk = 20)

  /**
   * HELPERS
   */

  def register(i: Int, messages: ListBuffer[String]) = {
    Thread.sleep(i.toLong)
    messages.append("got "+i)
    i
  }

}
