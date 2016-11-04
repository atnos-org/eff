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
    send[Async, R, A](AsyncTaskNow[A](a))

  def asyncFail[R :_async](t: Throwable): Eff[R, Unit] =
    send[Async, R, Unit](AsyncTaskFailed(t))

  def asyncDelay[R :_async, A](a: =>A): Eff[R, A] =
    send[Async, R, A](AsyncTaskDelayed(() => a))

  def asyncFork[R :_async, A](a: =>A): Eff[R, A] =
    fork(AsyncTaskDelayed(() => a))

  def fork[R :_async, A](a: =>Async[A]): Eff[R, A] =
    asyncDelay {
      a match {
        case AsyncTask(es1, t) => create(Task.fork(t(es))(es1))
        case _                => send(a)
      }
    }.flatten

  def create[R :_async, A](task: Task[A]): Eff[R, A] =
    send[Async, R, A](AsyncTask(es, {_ => task}))
}

trait AsyncTaskServiceInterpretation {

  def runAsyncTask[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    e.detachA(ApplicativeAsync) match {
      case AsyncTaskNow(a)     => Task.now(a)
      case AsyncTaskDelayed(a) => Either.catchNonFatal(a()).fold(Task.fail, Task.now)
      case AsyncTaskFailed(t)  => Task.fail(t)
      case AsyncTask(es, run)  => run(es)
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

    def pure[A](a: A) = AsyncTaskNow(a)

    def ap[A, B](ff: Async[A => B])(fa: Async[A]): Async[B] =
      fa match {
        case AsyncTaskNow(a) =>
          ff match {
            case AsyncTaskNow(f)     => AsyncTaskNow(f(a))
            case AsyncTaskFailed(t)  => AsyncTaskFailed(t)
            case AsyncTaskDelayed(f) => AsyncTaskDelayed(() => f()(a))
            case AsyncTask(esf, f)   => AsyncTask(esf, { implicit es => f(es).map(_(a))})
          }

        case AsyncTaskFailed(t) => AsyncTaskFailed(t)

        case AsyncTaskDelayed(a) =>
          ff match {
            case AsyncTaskNow(f)     => AsyncTaskDelayed(() => f(a()))
            case AsyncTaskFailed(t)  => AsyncTaskFailed(t)
            case AsyncTaskDelayed(f) => AsyncTaskDelayed(() => f()(a()))
            case AsyncTask(esf, f)   => AsyncTask(esf, { implicit es => f(es).map(_(a()))})
          }

        case AsyncTask(eca, a) =>
          AsyncTask(eca, { implicit es =>
            val app = TaskApplicative
            ff match {
              case AsyncTaskNow(f)     => a(es).map(f)
              case AsyncTaskFailed(t)  => Task.fail(t)
              case AsyncTaskDelayed(f) => a(es).map(x => f()(x))
              case AsyncTask(esf, f)   => app.ap(f(esf))(a(es))
            }
          })
      }

    override def toString = "Applicative[AsyncTask]"
  }

  implicit def MonadAsync: Monad[Async] = new Monad[Async] {
    def pure[A](a: A) = AsyncTaskNow(a)

    def flatMap[A, B](aa: Async[A])(af: A => Async[B]): Async[B] =
      aa match {
        case AsyncTaskNow(a)     => af(a)
        case AsyncTaskDelayed(a) => af(a())
        case AsyncTaskFailed(t)  => AsyncTaskFailed(t)

        case AsyncTask(esa, fa) =>
          AsyncTask(esa, { implicit es =>
            fa(es).flatMap { a =>
              af(a) match {
                case AsyncTaskNow(b)     => Task.now(b)
                case AsyncTaskFailed(t)  => Task.fail(t)
                case AsyncTaskDelayed(b) => try Task.now(b()) catch { case NonFatal(t) => Task.fail(t) }
                case AsyncTask(esb, fb)  => fb(esb)
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

case class AsyncTaskNow[A](a: A) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncTaskNow(Right(a))
}

case class AsyncTaskFailed[A](t: Throwable) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncTaskNow(Left(t))
}

case class AsyncTaskDelayed[A](run: () => A) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncTaskDelayed(() => Either.catchNonFatal(run()))
}

case class AsyncTask[A](es: ExecutorService, run: ExecutorService => Task[A]) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncTask(es, { es => run(es).attempt.map(_.fold(Either.left, Either.right)) })
}

