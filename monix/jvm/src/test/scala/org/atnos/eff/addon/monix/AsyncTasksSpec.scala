package org.atnos.eff.addon.monix

import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.async._
import org.atnos.eff.syntax.all._
import org.atnos.eff.addon.monix.AsyncTasks._
import org.scalacheck._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.ThrownExpectations

import scala.collection.mutable.ListBuffer
import monix.execution.Scheduler.Implicits.global
import monix.eval.Task
import org.atnos.eff._

import scala.concurrent.Await
import scala.concurrent.duration._

class AsyncTasksSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations { def is = "monix task".title ^ sequential ^ s2"""

 Async effects can be implemented with an AsyncTask service $e1
 Async effects can be attempted                             $e2
 Async effects can be executed concurrently                 $e3
 Async effects are stacksafe                                $e4
 Async effects can trampoline a Task                        $e5
 An Async forked computation can be timed out               $e6

 Simple Async calls can be memoized                 $e7
 Attempted Async calls can be memoized              $e8
 Simple Async calls with timeout can be memoized    $e9
 Attempted Async calls with timeout can be memoized $e10

 Async calls with flatMaps can be memoized with a memo effect $e11
   if 'def' is used to create the computation, then a unique key must be provided $e12

"""

  type S = Fx.fx2[Async, Option]

  def e1 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork(20)
    } yield a + b

    action[S].runOption.runAsync.runAsync must beSome(30).await(retries = 5, timeout = 5.seconds)
  }

  def e2 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork { boom; 20 }
    } yield a + b

    action[S].asyncAttempt.runOption.runAsync.runAsync must beSome(beLeft(boomException)).await(retries = 5, timeout = 5.seconds)
  }

  def e3 = {
    val messages: ListBuffer[Int] = new ListBuffer[Int]
    val delays = List(600, 200, 400, 800)

    def action[R :_async](i: Int): Eff[R, Int] =
      asyncFork {
        Thread.sleep(i.toLong)
        messages.append(i)
        i
      }

    val run = asyncFork[S, Unit](Thread.sleep(1000)) >> Eff.traverseA(delays)(action[S])

    eventually(retries = 5, sleep = 0.seconds) {
      messages.clear
      Await.result(run.runOption.runAsync.runAsync, 3.seconds)

      "the messages are ordered" ==> {
        messages.toList ==== delays.sorted
      }
    }

  }

  def e4 = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => asyncFork[S, String](i.toString))

    action.runOption.runAsync.runAsync must beSome(list.map(_.toString)).await(retries = 5, timeout = 5.seconds)
  }

  def e5 = {
    type R = Fx.fx1[Async]

    def loop(i: Int): Task[Eff[R, Int]] =
      if (i == 0) Task.now(Eff.pure(1))
      else Task.now(suspend(loop(i - 1)).map(_ + 1))

    Await.result(suspend(loop(100000)).runAsync.runAsync, Duration.Inf) must not(throwAn[Exception])
  }

  def e6 = {
    asyncFork({ sleepFor(10000.millis); 1 }, timeout = Option(50.millis)).asyncAttempt.runAsync.runAsync must beLeft[Throwable].awaitFor(20.seconds)
  }

  def e7 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    val makeRequest = asyncMemo(cache)(asyncFork({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runAsync.runAsync must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e8 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    val makeRequest = asyncMemo(cache)(asyncFork({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).asyncAttempt.runAsync.runAsync must beRight(1).await
    invocationsNumber must be_==(1)
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    val makeRequest = asyncMemo(cache)(asyncFork({ invocationsNumber += 1; 1 }, timeout = Option(10000.millis)))

    (makeRequest >> makeRequest).runAsync.runAsync must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    val makeRequest = asyncFork({ invocationsNumber += 1; 1 }, timeout = Option(100.seconds)).asyncMemo(cache)
      (makeRequest >> makeRequest).asyncAttempt.runAsync.runAsync must beRight(1).await

    invocationsNumber must be_==(1)
  }

  def e11 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx2[Memoized, Async]
    val intAsync: Eff[S, Int] = asyncFork[S, String]("a").flatMap(_ => asyncFork[S, Int]({ invocationsNumber += 1; 1 }))
    val makeRequest: Eff[S, Int] = asyncMemoized(intAsync)

    runAsyncMemo(cache)(makeRequest >> makeRequest).runAsync.runAsync must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e12 = prop { i: Int =>
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx2[Memoized, Async]
    val intAsync: Eff[S, Int] = asyncFork[S, String]("a").flatMap(_ => asyncFork[S, Int] { invocationsNumber += 1; 1 })

    // with a 'def' we need to specify a key to store the request async values
    def makeRequest: Eff[S, Int] = asyncMemoized("key", intAsync)

    runAsyncMemo(cache)(makeRequest >> makeRequest).runAsync.runAsync must be_==(1).await
    invocationsNumber must be_==(1)
  }.set(minTestsOk = 10)


  /**
   * HELPERS
   */
  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")

  def sleepFor(duration: FiniteDuration) =
    try Thread.sleep(duration.toMillis) catch { case t: Throwable => () }

}

