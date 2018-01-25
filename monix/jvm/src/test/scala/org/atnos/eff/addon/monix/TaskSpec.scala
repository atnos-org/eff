package org.atnos.eff.addon.monix

import cats.effect.Async
import monix.eval._
import org.atnos.eff._
import org.atnos.eff.addon.monix.task._
import org.atnos.eff.syntax.addon.monix.task._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.ThrownExpectations
import fs2._
import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global

class TaskSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 Task effects can be used as normal values                 $e1

"""

  implicit val executorService = ExecutorServices.fromExecutorService(ee.es)

  def e1 = {
    Stream.repeatEval(taskDelay[S2, Unit](println("runs"))).take(1100).compile.drain.runAsync.map(_ => Right(1)).runAsync must beRight.awaitFor(20.seconds)

    ok
  }


  implicit def asyncEff[R, T](implicit taskE: _Task[R]): Async[Eff[R, ?]] = new Async[Eff[R, ?]] {

    def pure[A](a: A): Eff[R, A] =
      taskDelay(a)

    def handleErrorWith[A](fa: Eff[R, A])(f: Throwable => Eff[R, A]): Eff[R, A] =
      taskAttempt(fa).flatMap(d => d.fold(
        t => taskFailed(t),
        a => taskDelay(a)
      ))

    def raiseError[A](t: Throwable): Eff[R, A] =
      taskFailed(t)

    def flatMap[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Eff[R, Either[A, B]]): Eff[R, B] =
      Eff.EffMonad.tailRecM(a)(f)

    def suspend[A](thunk: => Eff[R, A]): Eff[R, A] =
      taskSuspend(Task.delay(thunk))

    def async[A](k: ((Either[Throwable, A]) => Unit) => Unit): Eff[R, A] =
      ??? // effAsync(k)
  }

  type S2 = Fx.fx1[Task]
  /**
   * HELPERS
   */
  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")

  def sleepFor(duration: FiniteDuration) =
    try Thread.sleep(duration.toMillis) catch { case t: Throwable => () }
}

