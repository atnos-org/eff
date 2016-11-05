package org.atnos.eff
package scalaz

import java.util.concurrent.ExecutorService

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import _root_.scalaz.concurrent._
import _root_.scalaz.Nondeterminism
import cats._
import cats.implicits._

import scala.util.control.NonFatal
import AsyncTaskService._

case class AsyncTaskService(esEval: Eval[ExecutorService]) extends AsyncService {

  implicit val es: ExecutorService = esEval.value

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    send[Async, R, A](AsyncTask[A](Task.now(a)))

  def asyncFail[R :_async](t: Throwable): Eff[R, Unit] =
    send[Async, R, Unit](AsyncTask(Task.fail(t)))

  def asyncDelay[R :_async, A](a: =>A): Eff[R, A] =
    send[Async, R, A](AsyncTask(Task.suspend(Task.delay(a))))

  def asyncFork[R :_async, A](a: =>A): Eff[R, A] =
    fork(AsyncTask(Task.delay(a)))

  def fork[R :_async, A](a: =>Async[A]): Eff[R, A] =
    asyncDelay {
      a match {
        case AsyncTask(t)          => create(Task.fork(t)(es))
        case AsyncTaskFork(es1, t) => create(Task.fork(t(es))(es1))
      }
    }.flatten

  def create[R :_async, A](task: Task[A]): Eff[R, A] =
    send[Async, R, A](AsyncTaskFork(es, { _ => task}))
}

trait AsyncTaskServiceInterpretation {

  def runAsyncTask[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    e.detachA(ApplicativeAsync) match {
      case AsyncTask(t) => t
      case AsyncTaskFork(es, run)  => run(es)
    }

  implicit class RunAsyncTaskOps[A](e: Eff[Fx.fx1[Async], A]) {
    def runAsyncTask: Task[A] =
      AsyncTaskService.runAsyncTask(e)
  }

}

object AsyncTaskServiceInterpretation extends AsyncTaskServiceInterpretation

object AsyncTaskService extends AsyncTaskServiceInterpretation {

  def create(implicit es: ExecutorService): AsyncTaskService =
    fromExecutorService(es)

  /** create an AsyncTaskervice but do not evaluate the executor service yet */
  def fromExecutorService(es: =>ExecutorService): AsyncTaskService =
    AsyncTaskService(Eval.later(es))

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

  implicit def MonadAsync: Monad[Async] = new Monad[Async] {
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

case class AsyncTask[A](task: Task[A]) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncTask(task.attempt.map(_.fold(Either.left, Either.right)))
}

case class AsyncTaskFork[A](es: ExecutorService, run: ExecutorService => Task[A]) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncTaskFork(es, { es => run(es).attempt.map(_.fold(Either.left, Either.right)) })
}

