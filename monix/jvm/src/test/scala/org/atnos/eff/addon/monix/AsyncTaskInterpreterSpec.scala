package org.atnos.eff.addon.monix

import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.addon.monix._
import org.scalacheck._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import monix.execution.Scheduler.Implicits.global
import monix.eval.Task
import org.atnos.eff._

import scala.concurrent.Await
import scala.concurrent.duration._

class AsyncTaskInterpreterSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck { def is = "monix task".title ^ s2"""

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

  def e3 = prop { ls: List[Int] =>
    val messages: ListBuffer[Int] = new ListBuffer[Int]

    def action[R :_async](i: Int): Eff[R, Int] =
      asyncFork {
        Thread.sleep(i.toLong)
        messages.append(i)
        i
      }

    val run = Eff.traverseA(ls)(i => action[S](i))

    eventually(retries = 5, sleep = 1.second) {
      messages.clear
      Await.result(run.runOption.runAsync.runAsync, 3.seconds)

      "the messages are ordered" ==> {
        messages.toList ==== ls.sorted
      }
    }

  }.set(minTestsOk = 10).setGen(Gen.const(scala.util.Random.shuffle(List(10, 200, 300, 400, 500))))

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

    Await.result(suspend(loop(100000)).runAsync.runAsync, 5 seconds) must not(throwAn[Exception])
  }

  def e6 = {
    lazy val slow = { sleepFor(200.millis); 1 }
    asyncFork(slow, timeout = Option(50.millis)).asyncAttempt.runAsync.runAsync must beLeft[Throwable].await
  }

  def e7 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = asyncMemo("only once", cache, asyncFork({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runAsync.runAsync must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e8 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = asyncMemo("only once", cache, asyncFork({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).asyncAttempt.runAsync.runAsync must beRight(1).await
    invocationsNumber must be_==(1)
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = asyncMemo("only once", cache, asyncFork({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))

    (makeRequest >> makeRequest).runAsync.runAsync must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    def makeRequest = asyncMemo("only once", cache, asyncFork({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))
    (makeRequest >> makeRequest).asyncAttempt.runAsync.runAsync must beRight(1).await

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

