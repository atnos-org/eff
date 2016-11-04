package org.atnos.eff
package monix

import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import cats._
import AsyncTaskService._
import _root_.monix.eval.Task
import _root_.monix.execution.Scheduler
import scala.util._

case class AsyncTaskService() extends AsyncService {

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    create(Task.now(a))

  def asyncFail[R :_async](t: Throwable): Eff[R, Unit] =
    create[R, Unit](Task.raiseError(t))

  def asyncDelay[R :_async, A](a: =>A): Eff[R, A] =
    create[R, A](Task.delay(a))

  def asyncFork[R :_async, A](a: =>A): Eff[R, A] =
    fork[R, A](AsyncTask(Task.delay(a)))

  def fork[R :_async, A](a: =>Async[A]): Eff[R, A] =
    asyncDelay { a match { case AsyncTask(t) => create(Task.fork(t)) } }.flatten

  def create[R :_async, A](t: Task[A]): Eff[R, A] =
    send[Async, R, A](AsyncTask(t))
}

trait AsyncTaskServiceInterpretation {

  def runAsyncTask[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    e.detachA(ApplicativeAsync) match {
      case AsyncTask(t) => t
    }

  implicit class RunAsyncTaskOps[A](e: Eff[Fx.fx1[Async], A]) {
    def runAsyncTask: Task[A] =
      AsyncTaskService.runAsyncTask(e)
  }

}

object AsyncTaskServiceInterpretation extends AsyncTaskServiceInterpretation

object AsyncTaskService extends AsyncTaskServiceInterpretation {

  def ApplicativeAsync: Applicative[Async] = new Applicative[Async] {

    def pure[A](a: A) = AsyncTask(Task.now(a))

    def ap[A, B](ff: Async[A => B])(fa: Async[A]): Async[B] =
      (ff, fa) match {
        case (AsyncTask(f), AsyncTask(a)) => AsyncTask(f.zipMap(a)(_(_)))
      }

    override def toString = "Applicative[AsyncTask]"
  }

  implicit def MonadAsync: Monad[Async] = new Monad[Async] {
    def pure[A](a: A) = AsyncTask(Task.now(a))

    def flatMap[A, B](at: Async[A])(f: A => Async[B]): Async[B] =
      AsyncTask(at match { case AsyncTask(t) => t.flatMap(a => f(a) match { case AsyncTask(ff) => ff }) })

    def tailRecM[A, B](a: A)(f: A => Async[Either[A, B]]): Async[B] =
      flatMap(f(a)) {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => pure(b)
      }

    override def toString = "Monad[AsyncTask]"
  }

  def TaskApplicative: Applicative[Task] = new Applicative[Task] {
    def pure[A](x: A): Task[A] =
      Task.now(x)

    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] =
      ff.zipMap(fa)(_(_))
  }

}

case class AsyncTask[A](task: Task[A]) extends Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncTask(task.materialize.map { case Success(a) => Right(a); case Failure(t) => Left(t) })
}

