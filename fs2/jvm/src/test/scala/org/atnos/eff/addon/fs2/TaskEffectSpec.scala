package org.atnos.eff.addon.fs2

import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.scalacheck._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import fs2._
import org.atnos.eff._

import scala.concurrent.Await
import scala.concurrent.duration._

class TaskEffectSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck { def is = "fs2 task".title ^ sequential ^ s2"""

 Tasks can work as normal values                           $e1
 Task effects can be attempted                             $e2
 Task effects can be executed concurrently                 $e3
 Task effects are stacksafe with recursion                 $e5
 A forked task computation can be timed out                $e6

 Simple Task calls can be memoized                 $e7
 Attempted Task calls can be memoized              $e8
 Simple Task calls with timeout can be memoized    $e9
 Attempted Task calls with timeout can be memoized $e10

 Async calls can be memoized with a memo effect $e13

"""

  implicit val scheduler = Scheduler.fromScheduledExecutorService(ee.ses)
  implicit val strategy = Strategy.fromExecutionContext(ee.ec)

  type S = Fx.fx2[TimedTask, Option]

  def e1 = {
    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- taskDelay(10)
      b <- taskDelay(20)
    } yield a + b

    action[S].runOption.runSequential.unsafeRunAsyncFuture() must beSome(30).await(retries = 5, timeout = 5.seconds)
  }

  def e2 = {
    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- taskDelay(10)
      b <- taskDelay { boom; 20 }
    } yield a + b

    action[S].taskAttempt.runOption.runSequential.unsafeRunAsyncFuture() must beSome(beLeft(boomException)).await(retries = 5, timeout = 5.seconds)
  }

  def e3 = {
    val messages: ListBuffer[Int] = new ListBuffer[Int]
    val delays = List(600, 200, 400, 800)

    def action(i: Int): Eff[S, Unit] =
      taskFork(Task.delay {
        Thread.sleep(i.toLong)
        messages.append(i)
      })

    val run = taskDelay[S, Unit](Thread.sleep(1000)) >> Eff.traverseA(delays)(action)

    eventually(retries = 5, sleep = 0.seconds) {
      messages.clear
      Await.result(run.runOption.runAsync.unsafeRunAsyncFuture(), 3.seconds)

      "the messages are ordered" ==> {
        messages.toList ==== delays.sorted
      }
    }

  }

  def e5 = {
    type R = Fx.fx1[TimedTask]

    def loop(i: Int): Task[Eff[R, Int]] =
      if (i == 0) Task.now(Eff.pure(1))
      else Task.now(taskSuspend(loop(i - 1)).map(_ + 1))

    Await.result(taskSuspend(loop(100000)).runSequential.unsafeRunAsyncFuture(), Duration.Inf) must not(throwAn[Exception])
  }

  def e6 = {
    taskDelay({ sleepFor(10000.millis); 1 }, timeout = Some(50.millis)).taskAttempt.runSequential.unsafeRunAsyncFuture() must beLeft[Throwable].awaitFor(20.seconds)
  }

  def e7 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runSequential.unsafeRunAsyncFuture() must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e8 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).taskAttempt.runSequential.unsafeRunAsyncFuture() must beRight(1).await
    invocationsNumber must be_==(1)
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = taskMemo("only once", cache, taskDelay({ invocationsNumber += 1; 1 }, timeout = Option(10000.millis)))

    (makeRequest >> makeRequest).runSequential.unsafeRunAsyncFuture() must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    def makeRequest = taskDelay({ invocationsNumber += 1; 1 }, timeout = Option(10000.millis)).taskMemo("only once", cache)
    (makeRequest >> makeRequest).taskAttempt.runSequential.unsafeRunAsyncFuture() must beRight(1).await

    invocationsNumber must be_==(1)
  }

  def e13 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx2[Memoized, TimedTask]
    def makeRequest = taskMemoized("only once", taskDelay[S, Int]({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runTaskMemo(cache).runSequential.unsafeRunAsyncFuture must be_==(1).await
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
