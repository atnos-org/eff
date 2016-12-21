package org.atnos.eff.addon.fs2

import java.util.concurrent.{ExecutorService, ScheduledExecutorService, TimeoutException}

import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats._
import cats.implicits._
import fs2.Task.Callback
import fs2.{Scheduler, Strategy, Task}
import org.atnos.eff.SubscribeEffect.{Callback => _, _}
import org.atnos.eff.all._
import org.atnos.eff._
import org.atnos.eff.Async._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Either, Left, Right}

case class AsyncTaskInterpreter(es: ExecutorServices) extends AsyncInterpreter[Task] { outer =>

  implicit val s: Strategy = Strategy.fromExecutionContext(es.executionContext)

  implicit val sc: Scheduler = Scheduler.fromScheduledExecutorService(es.scheduledExecutorService)

  def runAsync[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    run(e.detachA(ApplicativeAsync))

  def runSequential[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    run(e.detach)

  def suspend[R :_async, A](task: =>Task[Eff[R, A]])(implicit s: Strategy): Eff[R, A] =
    fromTask(task).flatten

  def fromTask[R :_async, A](task: => Task[A])(implicit s: Strategy): Eff[R, A] =
    subscribe[R, A](SimpleSubscribe(callback =>
    { task.unsafeRunAsync(callback) }),
      None)

  def run[A](r: Async[A]): Task[A] =
    r match {
      case AsyncNow(a) => Task.now(a)
      case AsyncFailed(t) => Task.fail(t)
      case AsyncDelayed(a) => Either.catchNonFatal(a.value).fold(Task.fail, Task.now)
      case AsyncEff(e, to) => subscribeToTask(e, to).detachA(AsyncTaskInterpreter.TaskApplicative)(AsyncTaskInterpreter.TaskMonad)
    }

  def subscribeToTaskNat(timeout: Option[FiniteDuration]) =
    new (Subscribe ~> Task) {
      def apply[X](subscribe: Subscribe[X]): Task[X] = {
        val registerTask = { (c: Callback[X]) => subscribe(c) }

        subscribe match {
          case SimpleSubscribe(s, Some((k, cache))) => cache.memo(k, apply(SimpleSubscribe(s)))
          case AttemptedSubscribe(s, Some((k, cache))) => cache.memo(k, apply(AttemptedSubscribe(s)))

          case _ =>
            timeout match {
              case None => Task.async(registerTask)

              case Some(to) =>
                subscribe match {
                  case SimpleSubscribe(_, _) =>
                    Task.async(registerTask).unsafeTimed(to)

                  case as @ AttemptedSubscribe(sub, _) =>

                    Task.ref[X].flatMap { f =>
                      sc.scheduleOnce(to) { f.set(Task.async((cb: Callback[X]) => cb(Right(Left(new TimeoutException))))); () }
                      val ta = Task.async { subscribe }
                      f.set(ta)
                      f.get
                    }
                }
            }
        }
      }
    }

  def subscribeToTask[A](e: Eff[Fx1[Subscribe], A], timeout: Option[FiniteDuration])(implicit m: Subscribe <= Fx1[Subscribe]): Eff[Fx1[Task], A] =
    interpret.transform[Fx1[Subscribe], Fx1[Task], NoFx, Subscribe, Task, A](e, subscribeToTaskNat(timeout))

  implicit class RunAsyncTaskOps[A](e: Eff[Fx.fx1[Async], A]) {
    def runAsync: Task[A] =
      outer.runAsync(e)

    def runSequential: Task[A] =
      outer.runSequential(e)
  }

}

object AsyncTaskInterpreter  {

  def create(implicit es: ExecutorService, s: ScheduledExecutorService): AsyncTaskInterpreter =
    fromExecutorServices(es, s)

  /** create an AsyncTaskervice but do not evaluate the executor service yet */
  def fromExecutorServices(es: =>ExecutorService, s: => ScheduledExecutorService): AsyncTaskInterpreter =
    AsyncTaskInterpreter(ExecutorServices.fromExecutorServices(es, s))

  def TaskApplicative: Applicative[Task] = new Applicative[Task] {
    def pure[A](x: A): Task[A] =
      Task.now(x)

    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] =
      ff.map2(fa)(_(_))

    override def toString = "Applicative[Task]"
  }

  implicit def TaskMonad: Monad[Task] = new Monad[Task] {
    def pure[A](x: A): Task[A] =
      Task.now(x)

    def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Task[Either[A, B]]): Task[B] =
      f(a).flatMap {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => pure(b)
      }

    override def toString = "Monad[Task]"

  }

}