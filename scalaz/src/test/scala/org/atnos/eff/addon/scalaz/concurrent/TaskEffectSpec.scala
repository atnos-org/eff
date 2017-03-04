package org.atnos.eff.addon.scalaz.concurrent

import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import org.atnos.eff.addon.scalaz.concurrent.TaskEffect._
import org.atnos.eff.syntax.addon.scalaz.task._

import scala.concurrent._
import duration._
import org.scalacheck._
import org.specs2.matcher.FutureMatchers._
import org.specs2.matcher.TaskMatchers._
import scalaz.concurrent.Task

class TaskEffectSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck { def is = "scalaz task".title ^ sequential ^ s2"""

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

  implicit val timer = ExecutorServices.timerFromScheduledExecutorService(ee.ses)
  implicit val ec = ee.ec

  def e1 = {
    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- taskDelay(10)
      b <- taskDelay(20)
    } yield a + b

    eventually(retries = 5, sleep = 0.seconds) {
      action[S].runOption.runSequential must returnValue(beSome(30))
    }
  }

  def e2 = {
    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- taskDelay(10)
      b <- taskDelay { boom; 20 }
    } yield a + b

    eventually(retries = 5, sleep = 0.seconds) {
      action[S].taskAttempt.runOption.runSequential must returnValue(beSome(beLeft(boomException)))
    }
  }

  def e3 = {
    val messages: ListBuffer[Int] = new ListBuffer[Int]
    val delays = List(600, 200, 400, 800)

    def action(i: Int): Eff[S, Unit] =
      taskFork(Task.delay {
        Thread.sleep(i.toLong)
        messages.append(i)
      })

    val actions = taskDelay[S, Unit](Thread.sleep(1000)) >> Eff.traverseA(delays)(action)

    eventually(retries = 5, sleep = 0.seconds) {
      messages.clear
      actions.runOption.runAsync.unsafePerformSync

      "the messages are ordered" ==> {
        messages.toList ==== delays.sorted
      }
    }
  }

  def e5 = {
    type R = Fx.fx1[TimedTask]

    def loop(i: Int): Task[Eff[R, Int]] =
      if (i == 0) Task.now(Eff.pure(1))
      else        Task.now(taskSuspend(loop(i - 1)).map(_ + 1))

    taskSuspend(loop(10000)).runSequential must returnValue(10001)
  }

  def e6 = {
    taskDelay({ sleepFor(10000.millis); 1 }, timeout = Option(50.millis)).taskAttempt.runSequential must returnValue(beLeft[Throwable])
  }

  def e7 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runSequential must returnValue(1)
    invocationsNumber must be_==(1)
  }

  def e8 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).taskAttempt.runSequential must returnValue(Right(1))
    invocationsNumber must be_==(1)
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }, timeout = Option(10000.millis)))

    (makeRequest >> makeRequest).runSequential must returnValue(1)
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }, timeout = Option(10000.millis)))
    (makeRequest >> makeRequest).taskAttempt.runSequential must returnValue(Right(1))

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

