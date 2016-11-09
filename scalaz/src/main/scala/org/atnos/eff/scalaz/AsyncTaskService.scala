package org.atnos.eff
package scalaz

import java.util.concurrent.{ExecutorService, ScheduledExecutorService}

import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import _root_.scalaz.\/
import _root_.scalaz.concurrent._
import _root_.scalaz.Nondeterminism
import cats._
import cats.implicits._

import AsyncTaskService._

import scala.concurrent.duration.FiniteDuration

case class AsyncTaskService(executors: ExecutorServices) extends AsyncService {

  implicit val executorService: ExecutorService =
    executors.executorService

  implicit val scheduledExecutorService: ScheduledExecutorService =
    executors.scheduledExecutorService

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    send[Async, R, A](AsyncTask[A](Task.now(a)))

  def asyncFail[R :_async](t: Throwable): Eff[R, Unit] =
    send[Async, R, Unit](AsyncTask(Task.fail(t)))

  def asyncDelay[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    timeout match {
      case Some(to) =>
        send[Async, R, A](AsyncTask(Task.suspend(Task.delay(a)).timed(to)))

      case None =>
        send[Async, R, A](AsyncTask(Task.suspend(Task.delay(a))))
    }

  def asyncFork[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    fork(AsyncTask(Task.delay(a)), timeout)

  def fork[R :_async, A](a: =>Async[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    asyncDelay {
      a match {
        case AsyncTask(t)          => create(Task.fork(t)(executorService), timeout)
        case AsyncTaskFork(es1, t) => create(Task.fork(t(executorService))(es1), timeout)
      }
    }.flatten

  def suspend[R :_async, A](task: =>Task[Eff[R, A]]): Eff[R, A] =
    send[Async, R, Eff[R, A]](AsyncTask(Task.suspend(task))).flatten

  def async[R :_async, A](callback: ((Throwable Either A) => Unit) => Unit, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    create(Task.async((f: Throwable \/ A => Unit) => callback(ta => f(\/.fromEither(ta)))), timeout)

  def create[R :_async, A](task: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    timeout match {
      case Some(to) =>
        send[Async, R, A](AsyncTaskFork(executorService, { _ => task.timed(to) }))

      case None =>
        send[Async, R, A](AsyncTaskFork(executorService, { _ => task }))
    }
}

trait AsyncTaskServiceInterpretation {

  def runAsyncTask[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    e.detachA(ApplicativeAsync) match {
      case AsyncTask(t) => t
      case AsyncTaskFork(es, run)  => run(es)
    }

  def runTask[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    e.detach match {
      case AsyncTask(t) => t
      case AsyncTaskFork(es, run)  => run(es)
    }

  implicit def toRunAsyncTaskOps[A](e: Eff[Fx.fx1[Async], A]): RunAsyncTaskOps[A] = new RunAsyncTaskOps[A](e)

}

final class RunAsyncTaskOps[A](val e: Eff[Fx.fx1[Async], A]) extends AnyVal {
  def runAsyncTask: Task[A] =
    AsyncTaskService.runAsyncTask(e)

  def runTask: Task[A] =
    AsyncTaskService.runTask(e)
}

object AsyncTaskServiceInterpretation extends AsyncTaskServiceInterpretation

object AsyncTaskService extends AsyncTaskServiceInterpretation {

  def create(implicit es: ExecutorService, s: ScheduledExecutorService): AsyncTaskService =
    fromExecutorServices(es, s)

  /** create an AsyncTaskervice but do not evaluate the executor service yet */
  def fromExecutorServices(es: =>ExecutorService, s: =>ScheduledExecutorService): AsyncTaskService =
    AsyncTaskService(ExecutorServices.fromExecutorServices(es, s))

  def ApplicativeAsync: Applicative[Async] = new Applicative[Async] {

    def pure[A](a: A) = AsyncTask(Task.now(a))

    def ap[A, B](ff: Async[A => B])(fa: Async[A]): Async[B] =
      fa match {
        case AsyncTask(a) =>
          ff match {
            case AsyncTask(f)          => AsyncTask(TaskApplicative.ap(f)(a))
            case AsyncTaskFork(esf, f) => AsyncTaskFork(esf, { es => TaskApplicative.ap(f(es))(a)})
          }

        case AsyncTaskFork(eca, a) =>
          ff match {
            case AsyncTask(f)          => AsyncTaskFork(eca, { es => TaskApplicative.ap(f)(a(es))})
            case AsyncTaskFork(esf, f) => AsyncTaskFork(esf, { es => TaskApplicative.ap(f(es))(a(es))})
          }
      }

    override def toString = "Applicative[AsyncTask]"
  }

  implicit final def MonadAsync: Monad[Async] = new Monad[Async] {
    def pure[A](a: A) = AsyncTask(Task.now(a))

    def flatMap[A, B](aa: Async[A])(af: A => Async[B]): Async[B] =
      aa match {
        case AsyncTask(t) => AsyncTask(t.flatMap(a => af(a) match {
          case AsyncTask(s) => s
          case AsyncTaskFork(es, s) => s(es)
        }))

        case AsyncTaskFork(esa, fa) =>
          AsyncTaskFork(esa, { implicit es =>
            fa(es).flatMap { a =>
              af(a) match {
                case AsyncTask(t)           => t
                case AsyncTaskFork(esb, fb) => fb(esb)
              }
            }})
      }

    def tailRecM[A, B](a: A)(f: A => Async[Either[A, B]]): Async[B] =
      f(a).flatMap {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => pure(b)
      }

    override def toString = "Monad[AsyncTask]"
  }

  def TaskApplicative: Applicative[Task] = new Applicative[Task] {
    def pure[A](x: A): Task[A] =
      Task.now(x)

    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] =
      Nondeterminism[Task].mapBoth(ff, fa)((f, a) => f(a))
  }

}

case class AsyncTask[A](task: Task[A]) extends AnyVal with Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncTask(task.attempt.map(_.fold(Either.left, Either.right)))
}

case class AsyncTaskFork[A](es: ExecutorService, run: ExecutorService => Task[A]) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncTaskFork(es, { es => run(es).attempt.map(_.fold(Either.left, Either.right)) })
}

