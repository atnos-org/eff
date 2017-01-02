package org.atnos.eff.addon.twitter

import scala.collection.mutable.ListBuffer
import scala.concurrent._
import scala.concurrent.duration._
import cats.implicits._
import com.twitter
import com.twitter.util.{Await, Future}
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.scalacheck._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.ThrownExpectations

class AsyncTwitterFutureInterpreterSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 Async effects can be implemented with an AsyncFuture service                       $e1
 Async effects can be attempted                                                     $e2
 Async effects can be executed concurrently                                         $e3
 Async effects are stacksafe with asyncFork                                         $e4
 Async effects are stacksafe with asyncDelay                                        $e5
 Async effects can trampoline a Task                                                $e6
 An Async effect can be created from Either                                         $e7
 An Async forked computation can be timed out                                       $e8
 An Async forked computation which throws an exception can be executed concurrently $e9

 Simple Async calls can be memoized                 $e10
 Attempted Async calls can be memoized              $e11
 Simple Async calls with timeout can be memoized    $e12
 Attempted Async calls with timeout can be memoized $e13

 Async calls can be memoized with a memo effect $e14

 Async calls with flatMaps can be memoized with a memo effect $e15
   if 'def' is used to create the computation, then a unique key must be provided $e16

"""

  type S = Fx.fx2[Async, Option]

  lazy val asyncService = AsyncTwitterFutureInterpreter()
  import asyncService._

  def e1 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork(20)
    } yield a + b

    Await.result(action[S].runOption.runAsyncFuture) must beSome(30)
  }

  def e2 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork { boom; 20 }
    } yield a + b

    Await.result(action[S].asyncAttempt.runOption.runAsyncFuture) must beSome(beLeft(boomException))
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
      Await.result(run.runOption.runAsyncFuture)

      "the messages are ordered" ==> {
        messages.toList ==== ls.sorted
      }
    }
  }.set(minTestsOk = 5).setGen(Gen.const(scala.util.Random.shuffle(List(10, 200, 300, 400, 500))))

  def e4 = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => asyncFork[S, String](i.toString))

    Await.result(action.runOption.runAsyncFuture) must beSome(list.map(_.toString))
  }

  def e5 = {
    val list = (1 to 5000).toList
    type U = Fx.prepend[Choose, S]
    val action = list.traverse(i => chooseFrom[U, Int](List(1)) >> asyncDelay[U, String](i.toString))

    Await.result(action.runChoose[List].runOption.map(_.map(_.flatten)).runAsyncFuture) must beSome(list.map(_.toString))
  }

  def e6 = {
    type R = Fx.fx1[Async]

    def loop(i: Int): Future[Eff[R, Int]] =
      if (i == 0) Future(Eff.pure(1))
      else        Future(suspend(loop(i - 1)).map(_ + 1))

    eventually(retries = 5, sleep = 1.second) {
      Await.result(suspend(loop(100000)).runAsyncFuture) must not(throwAn[Exception])
    }
  }

  def e7 = {
    Await.result(asyncFromEither(Left[Throwable, Int](boomException)).asyncAttempt.runAsyncFuture) must beLeft(boomException)
  }

  def e8 = {
    lazy val slow = { sleepFor(200.millis); 1 }
    Await.result(asyncFork(slow, timeout = Option(50.millis)).asyncAttempt.runAsyncFuture) must beLeft
  }

  def e9 = {
    type S = Fx.fx1[Async]
    def makeRequest[R: _async] = asyncFork[R, Unit](boom)

    { Await.ready(
      makeRequest[S].runAsyncFuture,
      twitter.util.Duration.fromSeconds(1)
    ) } must not(throwAn[Exception])
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    val makeRequest = asyncMemo(cache)(asyncFork({invocationsNumber += 1; 1}))

    Await.result((makeRequest >> makeRequest).runAsyncFuture) must be_==(1)
    invocationsNumber must be_==(1)
  }

  def e11 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    val makeRequest = asyncMemo(cache)(asyncFork({ invocationsNumber += 1; 1 }))

    Await.result((makeRequest >> makeRequest).asyncAttempt.runAsyncFuture) must beRight(1)
    invocationsNumber must be_==(1)
  }

  def e12 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    val makeRequest = asyncMemo(cache)(asyncFork({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))

    Await.result((makeRequest >> makeRequest).runAsyncFuture) must be_==(1)
    invocationsNumber must be_==(1)
  }

  def e13 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    val makeRequest = asyncMemo(cache)(asyncFork({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))
    Await.result((makeRequest >> makeRequest).asyncAttempt.runAsyncFuture) must beRight(1)

    invocationsNumber must be_==(1)
  }

  def e14 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx2[Memoized, Async]
    val makeRequest = asyncMemoized(asyncFork[S, Int]({ invocationsNumber += 1; 1 }))

    Await.result((makeRequest >> makeRequest).runAsyncMemo(cache).runAsyncFuture) must be_==(1)
    invocationsNumber must be_==(1)
  }

  def e15 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx2[Memoized, Async]
    val intAsync: Eff[S, Int] = asyncFork[S, String]("a").flatMap(_ => asyncFork[S, Int]({ invocationsNumber += 1; 1 }))
    val makeRequest: Eff[S, Int] = asyncMemoized(intAsync)

    Await.result(runAsyncMemo(cache)(makeRequest >> makeRequest).runAsyncFuture) must be_==(1)
    invocationsNumber must be_==(1)
  }

  def e16 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx2[Memoized, Async]
    val intAsync: Eff[S, Int] = asyncFork[S, String]("a").flatMap(_ => asyncFork[S, Int]({ invocationsNumber += 1; 1 }))

    // with a 'def' we need to specify a key to store the request async values
    def makeRequest: Eff[S, Int] = asyncMemoized("key", intAsync)

    Await.result(runAsyncMemo(cache)(makeRequest >> makeRequest).runAsyncFuture) must be_==(1)
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
