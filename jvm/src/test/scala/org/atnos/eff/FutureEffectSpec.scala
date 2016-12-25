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

import org.specs2.matcher.ThrownExpectations

import scala.util.control._

class FutureEffectSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 Future effects can work as normal values                      $e1
 Future effects can be attempted                               $e2
 Future effects can be executed concurrently                   $e3
 Future effects are stacksafe with recursion                   $e6
 An Future effect can be created from Either                   $e7
 An Future forked computation can be timed out                 $e8

 Simple Future calls can be memoized                 $e9
 Attempted Future calls can be memoized              $e10
 Simple Future calls with timeout can be memoized    $e11
 Attempted Future calls with timeout can be memoized $e12

 TimedFuture calls can be memoized with a memo effect $e10


"""

  type S = Fx.fx2[TimedFuture, Option]

  def e1 = {
    def action[R :_future :_option]: Eff[R, Int] = for {
      a <- futureDelay(10)
      b <- futureDelay(20)
    } yield a + b

    action[S].runOption.detach.runNow(ee.ses, ee.ec) must beSome(30).await(retries = 5, timeout = 5.seconds)
  }

  def e2 = {
    def action[R :_future :_option]: Eff[R, Int] = for {
      a <- futureDelay(10)
      b <- futureDelay { boom; 20 }
    } yield a + b

    action[S].futureAttempt.runOption.detach.runNow(ee.ses, ee.ec) must beSome(beLeft(boomException)).await(retries = 5, timeout = 5.seconds)
  }

  def e3 = prop { ls: List[Int] =>
    val messages: ListBuffer[Int] = new ListBuffer[Int]

    def action(i: Int): Eff[S, Unit] =
      futureDelay {
        Thread.sleep(i.toLong)
        messages.append(i)
      }

    val run = Eff.traverseA(ls)(action)
    eventually(retries = 1, sleep = 2.second) {
      messages.clear
      Await.result(run.runOption.detachA(TimedFuture.ApplicativeTimedFuture).runNow(ee.ses, ee.ec), 1 seconds)

      "the messages are ordered" ==> {
        messages.toList ==== ls.sorted
      }
    }
  }.set(minTestsOk = 5).setGen(Gen.const(scala.util.Random.shuffle(List(10, 200, 300, 400, 500))))

  def e5 = {
    val list = (1 to 5000).toList
    type U = Fx.prepend[Choose, S]
    val action = list.traverseA(i => chooseFrom[U, Int](List(1)) >> futureDelay[U, String](i.toString))

    action.runChoose[List].runOption.map(_.map(_.flatten)).detach.runNow(ee.ses, ee.ec) must beSome(list.map(_.toString)).await(retries = 5, timeout = 5.seconds)
  }

  def e6 = {
    type R = Fx.fx1[TimedFuture]

    def loop(i: Int): Future[Eff[R, Int]] =
      if (i == 0) Future.successful(Eff.pure(1))
      else        Future.successful(futureDefer(loop(i - 1)).flatten[Int].map(_ + 1))

    eventually(retries = 5, sleep = 1.second) {
      Await.result(futureDelay(loop(100000)).detach.runNow(ee.ses, ee.ec), 10.seconds) must not(throwAn[Exception])
    }
  }

  def e7 = {
    futureFromEither(Left[Throwable, Int](boomException)).futureAttempt.detach.runNow(ee.ses, ee.ec) must beLeft(boomException).awaitFor(1.second)
  }

  def e8 = {
    lazy val slow = { sleepFor(200.millis); 1 }
    futureDelay(slow, timeout = Some(50.millis)).futureAttempt.detach.runNow(ee.ses, ee.ec) must beLeft.await
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).detach.runNow(ee.ses, ee.ec) must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).futureAttempt.detach.runNow(ee.ses, ee.ec) must beRight(1).await
    invocationsNumber must be_==(1)
  }

  def e11 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))

    (makeRequest >> makeRequest).detach.runNow(ee.ses, ee.ec) must be_==(1).await
    invocationsNumber must be_==(1)
  }

  def e12 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))
    (makeRequest >> makeRequest).futureAttempt.detach.runNow(ee.ses, ee.ec) must beRight(1).await

    invocationsNumber must be_==(1)
  }

  def e13 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx2[Memoized, TimedFuture]
    def makeRequest = futureMemoized("only once", futureDelay[S, Int]({ invocationsNumber += 1; 1 }))

    (makeRequest >> makeRequest).runFutureMemo(cache).detach.runNow(ee.ses, ee.ec) must be_==(1).await
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

