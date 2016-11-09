package org.atnos.eff
package asyncmonix

import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats._
import AsyncTaskService._
import monix.eval._
import monix.execution._

import scala.concurrent.duration.FiniteDuration
import scala.util._

case class AsyncTaskService() extends AsyncService {

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    create(Task.now(a))

  def asyncFail[R :_async](t: Throwable): Eff[R, Unit] =
    create[R, Unit](Task.raiseError(t))

  def asyncDelay[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    create[R, A](Task.delay(a), timeout)

  def asyncFork[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    fork[R, A](AsyncTask(Task.delay(a)), timeout)

  def fork[R :_async, A](a: =>Async[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    asyncDelay { a match { case AsyncTask(t) => create(Task.fork(t), timeout) } }.flatten

  def suspend[R :_async, A](task: =>Task[Eff[R, A]]): Eff[R, A] =
    send[Async, R, Eff[R, A]](AsyncTask(Task.suspend(task))).flatten

  def async[R :_async, A](register: ((Throwable Either A) => Unit) => Unit, timeout: Option[FiniteDuration] = None): Eff[R, A] = {
    def callback(c: Callback[A]) = (ta: Throwable Either A) =>
      ta match {
        case Left(t)  => c.onError(t)
        case Right(a) => c.onSuccess(a)
      }

    val registerTask = (s: Scheduler, c: Callback[A]) =>
      Cancelable(() => register(callback(c)))

    create(Task.async(registerTask), timeout)
  }

  def create[R :_async, A](t: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    timeout match {
      case Some(to) => send[Async, R, A](AsyncTask(t.timeout(to)))
      case None     => send[Async, R, A](AsyncTask(t))
    }
}

trait AsyncTaskServiceInterpretation {

  def runAsyncTask[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    e.detachA(ApplicativeAsync) match {
      case AsyncTask(t) => t
    }

  def runTask[A](e: Eff[Fx.fx1[Async], A]): Task[A] =
    e.detach match {
      case AsyncTask(t) => t
    }

  implicit class RunAsyncTaskOps[A](e: Eff[Fx.fx1[Async], A]) {
    def runAsyncTask: Task[A] =
      AsyncTaskService.runAsyncTask(e)

    def runTask: Task[A] =
      AsyncTaskService.runTask(e)
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

case class AsyncTask[A](task: Task[A]) extends AnyVal with Async[A] {
  def attempt: Async[Throwable Either A] =
    AsyncTask(task.materialize.map { case Success(a) => Right(a); case Failure(t) => Left(t) })
}

