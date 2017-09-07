package org.atnos.eff.addon.scalaz.concurrent

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import org.atnos.eff.addon.scalaz.task._
import org.atnos.eff.syntax.addon.scalaz.task._

import scala.concurrent._
import duration._
import org.specs2.matcher.TaskMatchers._
import org.specs2.matcher.ThrownExpectations

import scalaz.concurrent.Task

class TaskEffectSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations { def is = "scalaz task".title ^ sequential ^ s2"""

 Task effects can be used as normal values                 $e1
 Task effects can be attempted                             $e2
 Task effects can be executed concurrently                 $e3
 Task effects can trampoline a Task                        $e5
 An Task forked computation can be timed out               $e6

 Simple Task calls can be memoized                 $e7
 Attempted Task calls can be memoized              $e8
 Simple Task calls with timeout can be memoized    $e9
 Attempted Task calls with timeout can be memoized $e10
 Failed tasks must not be memoized                 $e11

 Last actions must be triggered in case of a failure $e12
 Task effect is stacksafe with traverseA             $e13

"""

  type S = Fx.fx2[TimedTask, Option]

  implicit val executorService = ExecutorServices.fromExecutorService(ee.es)

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

  def e11 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    var firstTime = true

    def makeRequest =
      if (firstTime)
        taskMemo("only once", cache, taskDelay { firstTime = false; throw new Exception("") } >> taskDelay({ invocationsNumber += 1; 1 }))
      else
        taskMemo("only once", cache, taskDelay { invocationsNumber += 1; 1 })

    makeRequest.taskAttempt.runSequential must returnValue(beLeft[Throwable])
    makeRequest.taskAttempt.runSequential must returnValue(Right(1))

    invocationsNumber must be_==(1)
  }

  def e12 = {
    type S = Fx2[TimedTask, Option]
    var lastActionDone = 0

    val action: Eff[S, Int] =
      for {
        i <- taskDelay[S, Int] { sleepFor(10.millis); 1 }
        _ <- taskFailed[S, Int](new Exception("boom"))
        j =  i + 1
      } yield j

    val execute: Eff[S, Int] =
      action.
        addLast(taskDelay[S, Unit](lastActionDone += 1))

    execute.runOption.runAsync must failWith[Exception]
    lastActionDone must beEqualTo(1)
  }

  def e13 = {
    val action = (1 to 10000).toList.traverseA { i =>
      taskDelay(i)
    }

    action.runAsync must not(throwA[Throwable])
  }

  /**
   * HELPERS
   */
  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")

  def sleepFor(duration: FiniteDuration) =
    try Thread.sleep(duration.toMillis) catch { case t: Throwable => () }
}

