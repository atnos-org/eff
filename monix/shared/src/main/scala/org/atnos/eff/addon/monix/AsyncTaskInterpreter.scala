package org.atnos.eff.addon.monix

import java.util.concurrent.{TimeUnit, TimeoutException}

import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats._
import implicits._
import monix.eval._
import monix.execution._
import org.atnos.eff.Async._
import org.atnos.eff._
import org.atnos.eff.SubscribeEffect.{Callback => _, _}

import scala.concurrent.duration.FiniteDuration
import scala.util._

trait AsyncTaskInterpreter extends AsyncInterpreter[Task] { outer =>

  def runAsync[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    run(e.detachA(ApplicativeAsync))

  def runSequential[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    run(e.detach)

  def suspend[R :_async, A](task: =>Task[Eff[R, A]])(implicit s: Scheduler): Eff[R, A] =
    fromTask(task).flatten

  def fromTask[R :_async, A](task: =>Task[A])(implicit s: Scheduler): Eff[R, A] =
    subscribe[R, A](SimpleSubscribe(callback =>
    { task.map(a => callback(Right(a))).onErrorHandle(t => callback(Left(t))).runAsync; () }),
      None)

  def run[A](r: Async[A]): Task[A] =
    r match {
      case AsyncNow(a) => Task.now(a)
      case AsyncFailed(t) => Task.raiseError(t)
      case AsyncDelayed(a) => Either.catchNonFatal(a.value).fold(Task.raiseError, Task.now)
      case AsyncEff(e, to) => subscribeToTask(e, to).detachA(AsyncTaskInterpreter.TaskApplicative)(AsyncTaskInterpreter.TaskMonad)
    }

  def subscribeToTaskNat(timeout: Option[FiniteDuration]) =
    new (Subscribe ~> Task) {
      def apply[X](subscribe: Subscribe[X]): Task[X] = {
        def callback(c: Callback[X]) = (ta: Throwable Either X) =>
          ta match {
            case Left(t)  => c.onError(t)
            case Right(a) => c.onSuccess(a)
          }
        val registerTask = { (_: Scheduler, c: Callback[X]) => subscribe(callback(c)); Cancelable.empty }

        timeout match {
          case None => Task.fork(Task.async(registerTask))

          case Some(to) =>
            subscribe match {
              case SimpleSubscribe(_) =>
                Task.fork(Task.async(registerTask)).timeout(to)

              case as @ AttemptedSubscribe(sub) =>

                Task.fork(Task.async { (scheduler: Scheduler, cb: Callback[X]) =>
                  val cb1 = callback(cb)
                  val cancelable = Cancelable(() => subscribe(cb1))
                  scheduler.scheduleOnce(to) { cb1(Right(Left(new TimeoutException))); cancelable.cancel }
                  cancelable
                })
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

object AsyncTaskInterpreter extends AsyncTaskInterpreter {

  def TaskApplicative: Applicative[Task] = new Applicative[Task] {
    def pure[A](x: A): Task[A] =
      Task.now(x)

    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] =
      ff.zipMap(fa)(_(_))

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

