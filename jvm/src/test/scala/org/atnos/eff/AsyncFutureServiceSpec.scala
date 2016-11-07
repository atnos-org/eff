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

class AsyncFutureServiceSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck { def is = s2"""

 Async effects can be implemented with an AsyncFuture service $e1
 Async effects can be attempted                               $e2
 Async effects can be executed concurrently                   $e3
 Async effects are stacksafe                                  $e4
 Async effects can trampoline a Task                          $e5

"""

  type S = Fx.fx2[Async, Option]

  lazy val executorServices: ExecutorServices =
    ExecutorServices.fromExecutionContext(ee.executionContext)

  lazy val asyncService = AsyncFutureService(executorServices)
  import asyncService._

  def e1 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork(20)
    } yield a + b

    action[S].runOption.runAsyncFuture must beSome(30).await
  }

  def e2 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork { boom; 20 }
    } yield a + b

    action[S].asyncAttempt.runOption.runAsyncFuture must beSome(beLeft(boomException)).await
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
    Await.result(run.runOption.runAsyncFuture, 5 seconds)

    "the messages are ordered" ==> {
      messages.toList.sorted ==== ls.sorted
    }

  }.set(minTestsOk = 10).setGen(Gen.const(scala.util.Random.shuffle(List(10, 200, 300, 400, 500))))

  def e4 = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => asyncFork[S, String](i.toString))

    action.runOption.runAsyncFuture must beSome(list.map(_.toString)).await
  }

  def e5 = {
    type R = Fx.fx1[Async]

    def loop(i: Int): Future[Eff[R, Int]] =
      if (i == 0) Future.successful(Eff.pure(1))
      else        Future.successful(suspend(loop(i - 1)).map(_ + 1))

    Await.result(suspend(loop(100000)).runAsyncFuture, 3.seconds) must not(throwAn[Exception])
  }

  /**
   * HELPERS
   */

  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")


}

