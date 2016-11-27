package org.atnos.eff.addon.scalaz.concurrent

import java.util.concurrent.{ExecutorService, ScheduledExecutorService}

import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import scalaz.{-\/, Nondeterminism, \/, \/-}
import scalaz.concurrent._
import cats._
import cats.implicits._
import org.atnos.eff.SubscribeEffect._
import AsyncTaskInterpreter._
import org.atnos.eff._

import scala.concurrent.duration.FiniteDuration

case class AsyncTaskInterpreter(executors: ExecutorServices) extends AsyncInterpreter[Task] { outer =>

  val executorService: ExecutorService =
    executors.executorService

  val scheduledExecutorService: ScheduledExecutorService =
    executors.scheduledExecutorService

  def runAsync[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    run(e.detachA(Async.ApplicativeAsync))

  def runSequential[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    run(e.detach)

  def suspend[R :_async, A](task: =>Task[Eff[R, A]]): Eff[R, A] =
    fromTask(task).flatten

  def fromTask[R :_async, A](task: =>Task[A]): Eff[R, A] =
    subscribe[R, A](callback =>
    { task.unsafePerformAsync(ta => ta.fold(t => callback(Left(t)), a => callback(Right(a)))) } ,
      None)

  def run[A](r: Async[A]): Task[A] =
    r match {
      case AsyncNow(a) => Task.now(a)
      case AsyncFailed(t) => Task.fail(t)
      case AsyncDelayed(a, to) =>
        val task = Either.catchNonFatal(a.value).fold(Task.fail, Task.now)
        to.fold(task)(timeout => task.timed(timeout))

      case AsyncEff(e, to) => subscribeToTask(e, to).detachA(AsyncTaskInterpreter.TaskApplicative)
    }

  def subscribeToTaskNat(timeout: Option[FiniteDuration]) =
    new (Subscribe ~> Task) {
      def apply[X](subscribe: Subscribe[X]): Task[X] = {
        def callback(ta: (Throwable \/ X) => Unit) = (c: Throwable Either X) =>
          c match {
            case Left(t)  => ta(-\/(t))
            case Right(a) => ta(\/-(a))
          }

        val registerTask = (c: (Throwable \/ X) => Unit) =>
          subscribe(callback(c))

        timeout match {
          case Some(to) => Task.fork(Task.async(registerTask))(executorService).timed(to)(scheduledExecutorService)
          case None     => Task.fork(Task.async(registerTask))(executorService)
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

object AsyncTaskInterpreter {

  def create(implicit es: ExecutorService, s: ScheduledExecutorService): AsyncTaskInterpreter =
    fromExecutorServices(es, s)

  /** create an AsyncTaskervice but do not evaluate the executor service yet */
  def fromExecutorServices(es: =>ExecutorService, s: =>ScheduledExecutorService): AsyncTaskInterpreter =
    AsyncTaskInterpreter(ExecutorServices.fromExecutorServices(es, s))

  def TaskApplicative: Applicative[Task] = new Applicative[Task] {
    def pure[A](x: A): Task[A] =
      Task.now(x)

    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] =
      Nondeterminism[Task].mapBoth(ff, fa)(_(_))

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

