package org.atnos.eff

import cats.Eval
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import scala.concurrent._
import duration._
import org.scalacheck._
import Async._
import org.specs2.matcher.ThrownExpectations

import scala.util.control._

class AsyncFutureInterpreterSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 Async effects can be implemented with an AsyncFuture service $e1
 Async effects can be attempted                               $e2
 Async effects can be executed concurrently                   $e3
 Async effects are stacksafe with asyncFork                   $e4
 Async effects are stacksafe with asyncDelay                  $e5
 Async effects can trampoline a Task                          $e6
 An Async effect can be created from Either                   $e7
 An Async forked computation can be timed out                 $e8

 Simple Async calls can be memoized                 $e9
 Attempted Async calls can be memoized              $e10
 Simple Async calls with timeout can be memoized    $e11
 Attempted Async calls with timeout can be memoized $e12

 Async calls can be memoized with a memo effect $e10


"""

  type S = Fx.fx2[Async, Option]

  lazy val executorServices: ExecutorServices =
    ExecutorServices.fromExecutionContext(ee.executionContext)

  lazy val asyncService = AsyncFutureInterpreter(executorServices)
  import asyncService._

  def e1 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork(20)
    } yield a + b

    action[S].runOption.runAsyncFuture must beSome(30).await(retries = 5, timeout = 5.seconds)
  }

  def e2 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork { boom; 20 }
    } yield a + b

    action[S].asyncAttempt.runOption.runAsyncFuture must beSome(beLeft(boomException)).await(retries = 5, timeout = 5.seconds)
  }

  def e3 = prop { ls: List[Int] =>
    val messages: ListBuffer[Int] = new ListBuffer[Int]

    def action[R :_async](i: Int): Eff[R, Int] =
      asyncFork {
        Thread.sleep(i.toLong)
        messages.append(i)
        i
      }

    val run = Eff.traverseA(ls)(i => action[S](i))
    eventually(retries = 1, sleep = 1.second) {
      messages.clear
      Await.result(run.runOption.runAsyncFuture, 5 seconds)

      "the messages are ordered" ==> {
        messages.toList ==== ls.sorted
      }
    }
  }.set(minTestsOk = 5).setGen(Gen.const(scala.util.Random.shuffle(List(10, 200, 300, 400, 500))))

  def e4 = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => asyncFork[S, String](i.toString))

    action.runOption.runAsyncFuture must beSome(list.map(_.toString)).await(retries = 5, timeout = 5.seconds)
  }

  def e5 = {
    val list = (1 to 5000).toList
    type U = Fx.prepend[Choose, S]
    val action = list.traverse(i => chooseFrom[U, Int](List(1)) >> asyncDelay[U, String](i.toString))

    action.runChoose[List].runOption.map(_.map(_.flatten)).runAsyncFuture must beSome(list.map(_.toString)).await(retries = 5, timeout = 5.seconds)
  }

  def e6 = {
    type R = Fx.fx1[Async]

    def loop(i: Int): Future[Eff[R, Int]] =
      if (i == 0) Future.successful(Eff.pure(1))
      else        Future.successful(suspend(loop(i - 1)).map(_ + 1))

    eventually(retries = 5, sleep = 1.second) {
      Await.result(suspend(loop(100000)).runAsyncFuture, 10.seconds) must not(throwAn[Exception])
    }
  }

  def e7 = {
    asyncFromEither(Left[Throwable, Int](boomException)).asyncAttempt.runAsyncFuture must beLeft(boomException).awaitFor(1.second)
  }

  def e8 = {
    lazy val slow = { sleepFor(200.millis); 1 }
    asyncFork(slow, timeout = Option(50.millis)).asyncAttempt.runAsyncFuture must beLeft.await
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = asyncMemo("only once", cache, asyncFork({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runAsyncFuture must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = asyncMemo("only once", cache, asyncFork({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).asyncAttempt.runAsyncFuture must beRight(1).await
    invocationsNumber must be_==(1)
  }

  def e11 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = asyncMemo("only once", cache, asyncFork({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))

    (makeRequest >> makeRequest).runAsyncFuture must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e12 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    def makeRequest = asyncMemo("only once", cache, asyncFork({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))
    (makeRequest >> makeRequest).asyncAttempt.runAsyncFuture must beRight(1).await

    invocationsNumber must be_==(1)
  }

  def e13 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = asyncMemoized("only once", asyncFork({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runAsyncMemo(cache).runAsyncFuture must be_==(1).await
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

