package org.atnos.eff.addon.fs2

import cats.implicits._
import fs2.Task
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff._
import org.scalacheck._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration._

class AsyncTaskInterpreterSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck { def is = "fs2 task".title ^ s2"""

 Async effects can be implemented with an AsyncTask service   $e1
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

 Async calls can be memoized with a memo effect $e13


"""

  type S = Fx.fx2[Async, Option]

  lazy val executorServices: ExecutorServices =
    ExecutorServices.fromExecutionContext(ee.executionContext)

  lazy val asyncService = fromExecutorServices(executorServices)
  import asyncService._

  def e1 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork(20)
    } yield a + b

    action[S].runOption.runAsync.unsafeRunAsyncFuture must beSome(30).await(retries = 5, timeout = 5.seconds)
  }

  def e2 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork { boom; 20 }
    } yield a + b

    action[S].asyncAttempt.runOption.runAsync.unsafeRunAsyncFuture must beSome(beLeft(boomException)).await(retries = 5, timeout = 5.seconds)
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
      Await.result(run.runOption.runAsync.unsafeRunAsyncFuture, 5 seconds)

      "the messages are ordered" ==> {
        messages.toList ==== ls.sorted
      }
    }
  }.set(minTestsOk = 10).setGen(Gen.const(scala.util.Random.shuffle(List(10, 200, 300, 400, 500))))

  def e4 = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => asyncFork[S, String](i.toString))

    action.runOption.runAsync.unsafeRunAsyncFuture must beSome(list.map(_.toString)).await(retries = 5, timeout = 5.seconds)
  }

  def e5 = {
    val list = (1 to 5000).toList
    type U = Fx.prepend[Choose, S]
    val action = list.traverse(i => chooseFrom[U, Int](List(1)) >> asyncDelay[U, String](i.toString))

    action.runChoose[List].runOption.map(_.map(_.flatten)).runAsync.unsafeRunAsyncFuture must beSome(list.map(_.toString)).await(retries = 5, timeout = 5.seconds)
  }

  def e6 = {
    type R = Fx.fx1[Async]

    def loop(i: Int): Task[Eff[R, Int]] =
      if (i == 0) Task.now(Eff.pure(1))
      else        Task.now(suspend(loop(i - 1)).map(_ + 1))

    eventually(retries = 5, sleep = 1.second) {
      Await.result(suspend(loop(100000)).runAsync.unsafeRunAsyncFuture, 10.seconds) must not(throwAn[Exception])
    }
  }

  def e7 = {
    asyncFromEither(Left[Throwable, Int](boomException)).asyncAttempt.runAsync.unsafeRunAsyncFuture must beLeft(boomException).awaitFor(1.second)
  }

  def e8 = {
    lazy val slow = { sleepFor(200.millis); 1 }
    asyncFork(slow, timeout = Option(50.millis)).asyncAttempt.runAsync.unsafeRunAsyncFuture must beLeft.await
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = asyncMemo(asyncFork({ invocationsNumber += 1; 1 }), cache, "only once")

    (makeRequest >> makeRequest).runAsync.unsafeRunAsyncFuture must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = asyncMemo(asyncFork({ invocationsNumber += 1; 1 }), cache, "only once")

    (makeRequest >> makeRequest).asyncAttempt.runAsync.unsafeRunAsyncFuture must beRight(1).await
    invocationsNumber must be_==(1)
  }

  def e11 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = asyncMemo(asyncFork({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)), cache, "only once")

    (makeRequest >> makeRequest).runAsync.unsafeRunAsyncFuture must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e12 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    def makeRequest = asyncMemo(asyncFork({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)), cache, "only once")
    (makeRequest >> makeRequest).asyncAttempt.runAsync.unsafeRunAsyncFuture must beRight(1).await

    invocationsNumber must be_==(1)
  }

  def e13 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx2[Memoized, Async]
    def makeRequest = asyncMemoized("only once", asyncFork[S, Int]({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runAsyncMemo(cache).runAsync.unsafeRunAsyncFuture must be_==(1).await
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
