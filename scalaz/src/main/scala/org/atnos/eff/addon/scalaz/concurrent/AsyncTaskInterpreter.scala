package org.atnos.eff.addon.scalaz.concurrent

import java.util.concurrent._

import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import scalaz.{-\/, Nondeterminism, \/, \/-}
import scalaz.concurrent._
import cats._
import cats.implicits._
import org.atnos.eff.SubscribeEffect._
import AsyncTaskInterpreter._
import org.atnos.eff._

import scala.concurrent._, duration._
import scala.util.{Either, Success, Failure}

case class AsyncTaskInterpreter(executors: ExecutorServices) extends AsyncInterpreter[Task] { outer =>

  val executorService: ExecutorService =
    executors.executorService

  val scheduledExecutorService: ScheduledExecutorService =
    executors.scheduledExecutorService

  private lazy val futureInterpreter: AsyncFutureInterpreter =
    AsyncFutureInterpreter(executors)

  def runAsync[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    run(e.detachA(Async.ApplicativeAsync))

  def runSequential[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    run(e.detach)

  def suspend[R :_async, A](task: =>Task[Eff[R, A]]): Eff[R, A] =
    fromTask(task).flatten

  def fromTask[R :_async, A](task: =>Task[A]): Eff[R, A] =
    subscribe[R, A](SimpleSubscribe(callback =>
    { task.unsafePerformAsync(ta => ta.fold(t => callback(Left(t)), a => callback(Right(a)))) }) ,
      None)

  def run[A](r: Async[A]): Task[A] =
    r match {
      case AsyncNow(a) => Task.now(a)
      case AsyncFailed(t) => Task.fail(t)
      case AsyncDelayed(a) => Either.catchNonFatal(a.value).fold(Task.fail, Task.now)
      case AsyncEff(e, to) => subscribeToTask(e, to).detachA(AsyncTaskInterpreter.TaskApplicative)
    }

  def subscribeToTaskNat(timeout: Option[FiniteDuration]) =
    new (Subscribe ~> Task) {
      implicit val ec = executors.executionContext

      def apply[X](subscribe: Subscribe[X]): Task[X] = {

        subscribe.memoizeKey match {
          case Some((k, cache)) =>
            Task async { cb =>
              val future = futureInterpreter.subscribeToFutureNat(timeout)(subscribe)
              val memoized = cache.memo(k, future)

              memoized onComplete {
                case Success(a) => cb(\/-(a))
                case Failure(t) => cb(-\/(t))
              }
            }

          case None =>
            val registerTask = (tx: (Throwable \/ X) => Unit) =>
              subscribe((c: Throwable Either X) =>
                c match {
                  case Left(t)  => tx(-\/(t))
                  case Right(a) => tx(\/-(a))
                })

            timeout match {
              case None => Task.fork(Task.async(registerTask))

              case Some(to) =>
                subscribe match {
                  case SimpleSubscribe(_, _) =>
                    Task.fork(Task.async(registerTask)).timed(to)

                  case as @ AttemptedSubscribe(sub, _) =>
                    // there might be a more direct solution to reusing the Future
                    // interpreter but I don't know what it is
                    val future = futureInterpreter.subscribeToFutureNat(timeout)(as)

                    Task async { cb =>
                      future onComplete {
                        case Success(a) => cb(\/-(a))
                        case Failure(t) => cb(-\/(t))
                      }
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

