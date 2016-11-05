package org.atnos.eff
package monix

import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.monix._
import org.scalacheck._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv

import scala.collection.mutable.ListBuffer
import _root_.monix.execution.Scheduler.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

class AsyncTaskSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck { def is = s2"""

 Async effects can be implemented with an AsyncTask service $e1
 Async effects can be attempted                             $e2
 Async effects can be executed concurrently                 $e3
 Async effects are stacksafe                                $e4

"""

  type S = Fx.fx2[Async, Option]

  lazy val asyncService = AsyncTaskService()
  import asyncService._

  def e1 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork(20)
    } yield a + b

    action[S].runOption.runAsyncTask.runAsync must beSome(30).await
  }

  def e2 = {
    def action[R :_async :_option]: Eff[R, Int] = for {
      a <- asyncFork(10)
      b <- asyncFork { boom; 20 }
    } yield a + b

    action[S].asyncAttempt.runOption.runAsyncTask.runAsync must beSome(beLeft(boomException)).await
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
    Await.result(run.runOption.runAsyncTask.runAsync, 3 seconds)

    "the messages are ordered" ==> {
      messages.toList ==== ls.sorted
    }

  }.set(minTestsOk = 10).setGen(Gen.const(scala.util.Random.shuffle(List(10, 200, 300, 400, 500))))

  def e4 = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => asyncFork[S, String](i.toString))

    action.runOption.runAsyncTask.runAsync must beSome(list.map(_.toString)).await
  }

  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")

  def isSorted[T : Numeric](ls: List[T]): Boolean =
    ls.sorted == ls


}

