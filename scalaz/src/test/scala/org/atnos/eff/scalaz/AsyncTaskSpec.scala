package org.atnos.eff
package scalaz

import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.scalaz._

import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import _root_.scalaz.concurrent._
import scala.concurrent._, duration._
import org.scalacheck._
import org.specs2.matcher.TaskMatchers._

class AsyncTaskSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck { def is = "scalaz task".title ^ s2"""

 Async effects can be implemented with an AsyncTask service $e1
 Async effects can be attempted                             $e2
 Async effects can be executed concurrently                 $e3
 Async effects are stacksafe                                $e4
 Async effects can trampoline a Task                        $e5

"""

  type S = Fx.fx2[Async, Option]

  lazy val asyncService = AsyncTaskService.create(ee.executorService, ee.scheduledExecutorService)
  import asyncService._

  def e1 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork(20)
    } yield a + b

    action[S].runOption.runAsyncTask must returnValue(beSome(30))
  }

  def e2 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork { boom; 20 }
    } yield a + b

    action[S].asyncAttempt.runOption.runAsyncTask must returnValue(beSome(beLeft(boomException)))
  }

  def e3 = prop { ls: List[Int] =>

    val messages: ListBuffer[Int] = new ListBuffer[Int]

    def action[R :_async](i: Int): Eff[R, Int] =
      asyncFork {
        Thread.sleep(i.toLong)
        messages.append(i)
        i
      }

    val actions = Eff.traverseA(ls)(i => action[S](i))
    actions.runOption.runAsyncTask.unsafePerformSync

    "the messages are ordered" ==> {
       messages.toList ==== ls.sorted
    }

  }.set(minTestsOk = 5).setGen(Gen.const(scala.util.Random.shuffle(List(10, 200, 300, 400, 500))))

  def e4 = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => asyncFork[S, String](i.toString))

    action.runOption.runAsyncTask must returnValue(beSome(list.map(_.toString)))
  }

  def e5 = {
    type R = Fx.fx1[Async]

    def loop(i: Int): Task[Eff[R, Int]] =
      if (i == 0) Task.now(Eff.pure(1))
      else        Task.now(suspend(loop(i - 1)).map(_ + 1))

    suspend(loop(100000)).runTask must returnBefore(3.seconds)
  }

  /**
   * HELPERS
   */
  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")

}

