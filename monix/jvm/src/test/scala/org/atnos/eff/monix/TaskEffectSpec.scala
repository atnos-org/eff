package org.atnos.eff.monix

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.monix._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.monix._

import scala.concurrent._
import duration._
import cats.data._
import cats.Eval
import cats.implicits._
import _root_.monix.execution.Scheduler.Implicits.global
import _root_.monix.eval._
import org.specs2._
import org.scalacheck.Gen.{listOfN, choose => chooseInt}

import scala.collection.mutable.ListBuffer
import TaskEffect._
import org.scalacheck.Gen
import org.specs2.matcher.ThrownExpectations
import scala.concurrent.duration._

class TaskEffectSpec extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 A Task effect can be added to a stack of effects $e1
 A Task execution can be delayed $e2

 A Task value execution can be delayed $e3

 Tasks can be executed concurrently $e4
 Tasks can be executed sequentially $e5
 Tasks can be executed concurrently, using detachA to sequence them  $e6
 Tasks can be executed concurrently, embedded in a for comprehension $e7

 The task effect can be attempted $e8
 A timeout can be set             $e9
 A Task execution is stack-safe   $e10

"""

  def e1 = {
    type S = Fx.fx2[Task, Option]

    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- async(10)
      b <- option.some(a)
    } yield a + b

    action[S].runOption.awaitTask(1.second).run ==== Right(Some(20))

  }

  def e2 = {
    type S = Fx.fx2[Task, Eval]

    def action[R :_task :_eval]: Eff[R, Int] =
      delay(10).flatMap(v => async(v))

    action[S].runEval.awaitTask(1.second).run ==== Right(10)

  }

  def e3 = {
    type S = Fx.fx2[Task, Eval]

    def action[R :_task :_eval]: Eff[R, Int] =
      delay(Task(10)).flatMap(v => send(v))

    action[S].runEval.awaitTask(1.second).run ==== Right(10)

  }

  def e4 = prop { elements: List[Int] =>
    type S = Fx.fx1[Task]

    val messages = new ListBuffer[String]
    val actionApplicative: Eff[S, List[Int]] =
      elements.map(i => async(register(i, messages))).sequenceA

    actionApplicative.awaitTask(2.seconds).run ==== Right(elements)

    "messages are not received in the same order" ==> {
      messages.toList !=== elements.map("got "+_)
    }
  }.setGen(chooseInt(3, 5).flatMap(listOfN(_, chooseInt(10, 1000)))).
    set(minTestsOk = 1)

  def e5 = prop { elements: List[Int] =>
    type S = Fx.fx1[Task]

    val messages = new ListBuffer[String]
    val actionApplicative: Eff[S, List[Int]] =
      elements.map(i => async(register(i, messages))).sequence

    actionApplicative.awaitTask(2.seconds).run ==== Right(elements)

    "messages are received in the same order" ==> {
      messages.toList === elements.map("got "+_)
    }
  }.setGen(listGen).set(minTestsOk = 20)

  def e6 = prop { elements: List[Int] =>
    type S = Fx.fx1[Task]

    val messages = new ListBuffer[String]
    val actionApplicative: Eff[S, List[Int]] =
      elements.map(i => async(register(i, messages))).sequenceA

    val task = actionApplicative.detachA(ApplicativeTask)
    Await.result(task.runAsync, 2.seconds) ==== elements

    "messages are not received in the same order" ==> {
      messages.toList !=== elements.map("got "+_)
    }
  }.setGen(listGen).set(minTestsOk = 20)

  def e7 = prop { elements: List[Int] =>
    type S = Fx.fx2[ThrowableEither, Task]

    val messages = new ListBuffer[String]
    val actionApplicative: Eff[S, List[Int]] =
      elements.map(i => async[S, Int](register(i, messages))).sequenceA

    val action = for {
      i  <- either.right[S, Throwable, Int](1)
      is <- actionApplicative
      j  <- either.right[S, Throwable, Int](2)
    } yield i +: is :+ j

    val task = action.runEither.detachA(ApplicativeTask)
    Await.result(task.runAsync, 2.seconds) ==== Right(1 +: elements :+ 2)

    "messages are not received in the same order" ==> {
      messages.toList !=== elements.map("got "+_)
    }
  }.setGen(listGen).set(minTestsOk = 20)

  def e8 = prop { (elements: List[Int], string: String) =>
    type S = Fx.fx2[Reader[String, ?], Task]

    val actionOk: Eff[S, List[Int]] =
      for {
        s  <- ask[S, String]
        ts <- elements.traverseA[S, Int](i => async(i + s.size))
      } yield ts

    val taskOk = actionOk.attemptTask.runReader(string).detachA(ApplicativeTask)
    Await.result(taskOk.runAsync, 2.seconds) must beRight(elements.map(_ + string.size))

    val actionKo: Eff[S, List[Int]] =
      for {
        s  <- ask[S, String]
        ts <- elements.traverseA[S, Int](i => async(i + {throw new Exception("boom"); s.size}))
      } yield ts

    val taskKo = actionKo.attemptTask.runReader(string).detachA(ApplicativeTask)
    Await.result(taskKo.runAsync, 2.seconds) must beLeft

  }.setGen1(listGen).set(minTestsOk = 20)

  def e9 = prop { elements: List[Int] =>
    type S = Fx.fx1[Task]

    val messages = new ListBuffer[String]
    val actionApplicative: Eff[S, List[Int]] =
      elements.map(i => async { Thread.sleep(1000); register(i, messages) }).sequenceA

    val task = actionApplicative.withTimeout(100 millis).attemptTask.detachA(ApplicativeTask)
    Await.result(task.runAsync, 2.seconds) must beLeft[Throwable]((t: Throwable) => t must beAnInstanceOf[TimeoutException])
  }.setGen(listGen).set(minTestsOk = 1)

  def e10 = {
    def loop(i: Int): Task[Eff[Fx.fx1[Task], Int]] =
      if (i == 0) {
        Task.now(Eff.pure(1))
      } else {
        Task.now(monix.TaskEffect.suspend(loop(i - 1)).map(_ + 1))
      }

    Await.result(monix.TaskEffect.suspend(loop(100000)).detach.runAsync, 2.seconds) ==== 100001
  }

  /**
   * HELPERS
   */

  def listGen: Gen[List[Int]] =
    chooseInt(5, 10).flatMap(listOfN(_, chooseInt(10, 50)))

  def register(i: Int, messages: ListBuffer[String]) = {
    Thread.sleep(i.toLong)
    messages.append("got "+i)
    i
  }

}
