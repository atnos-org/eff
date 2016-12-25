package org.atnos.eff.addon.scalaz.concurrent

import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import org.atnos.eff.addon.scalaz.concurrent.TaskEffect._

import scala.concurrent._
import duration._
import org.scalacheck._
import org.specs2.matcher.TaskMatchers._

import scalaz.concurrent.Task

class TaskEffectSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck { def is = "scalaz task".title ^ s2"""

 Task effects can be used as normal values                 $e1
 Task effects can be attempted                             $e2
 Task effects can be executed concurrently                 $e3
 Task effects can trampoline a Task                        $e5
 An Task forked computation can be timed out               $e6

 Simple Task calls can be memoized                 $e7
 Attempted Task calls can be memoized              $e8
 Simple Task calls with timeout can be memoized    $e9
 Attempted Task calls with timeout can be memoized $e10

"""

  type S = Fx.fx2[TimedTask, Option]

  def e1 = {
    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- taskDelay(10)
      b <- taskDelay(20)
    } yield a + b

    eventually(retries = 5, sleep = 1.second) {
      action[S].runOption.detach.runNow(ee.ses, ee.ec) must returnValue(beSome(30))
    }
  }

  def e2 = {
    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- taskDelay(10)
      b <- taskDelay { boom; 20 }
    } yield a + b

    eventually(retries = 5, sleep = 1.second) {
      action[S].taskAttempt.runOption.detach.runNow(ee.ses, ee.ec) must returnValue(beSome(beLeft(boomException)))
    }
  }

  def e3 = prop { ls: List[Int] =>
    val messages: ListBuffer[Int] = new ListBuffer[Int]

    def action(i: Int): Eff[S, Unit] =
      taskFork(Task.delay {
        Thread.sleep(i.toLong)
        messages.append(i)
      })

    val actions = Eff.traverseA(ls)(action)

    eventually(retries = 5, sleep = 1.second) {
      messages.clear
      actions.runOption.detachA(TimedTask.TaskApplicative).runNow(ee.ses, ee.ec).unsafePerformSync

      "the messages are ordered" ==> {
        messages.toList ==== ls.sorted
      }
    }
  }.set(minTestsOk = 5).setGen(Gen.const(scala.util.Random.shuffle(List(10, 200, 300, 400, 500))))

  def e5 = {
    type R = Fx.fx1[TimedTask]

    def loop(i: Int): Task[Eff[R, Int]] =
      if (i == 0) Task.now(Eff.pure(1))
      else        Task.now(taskSuspend(loop(i - 1)).map(_ + 1))

    taskSuspend(loop(10000)).detach.runNow(ee.ses, ee.ec) must returnBefore(10.seconds)
  }

  def e6 = {
    lazy val slow = { sleepFor(200.millis); 1 }
    taskDelay(slow, timeout = Option(50.millis)).taskAttempt.detach.runNow(ee.ses, ee.ec) must returnValue(beLeft[Throwable])
  }

  def e7 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).detach.runNow(ee.ses, ee.ec) must returnValue(1)
    invocationsNumber must be_==(1)
  }

  def e8 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).taskAttempt.detach.runNow(ee.ses, ee.ec) must returnValue(Right(1))
    invocationsNumber must be_==(1)
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))

    (makeRequest >> makeRequest).detach.runNow(ee.ses, ee.ec) must returnValue(1)
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))
    (makeRequest >> makeRequest).taskAttempt.detach.runNow(ee.ses, ee.ec) must returnValue(Right(1))

    invocationsNumber must be_==(1)
  }


  /**
   * HELPERS
   */
  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")

  def sleepFor(duration: FiniteDuration) =
    try Thread.sleep(duration.toMillis) catch { case t: Throwable => () }

}

