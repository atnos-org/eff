package org.atnos.eff.monix

import scala.util.control.NonFatal
import cats._
import cats.data._
import cats.implicits._
import org.atnos.eff._
import Eff._
import Interpret._
import _root_.monix.eval._
import _root_.monix.execution._

import scala.concurrent._
import duration._
import TaskEffect._

/**
 * Effect for Task computations
 */
trait TaskEffect extends
  TaskCreation with
  TaskInterpretation

object TaskEffect extends TaskEffect {
  type _task[R] = Task |= R
  type _Task[R] = Task <= R
}

trait TaskCreation {
  def sync[R :_task, A](a: A): Eff[R, A] =
    pure(a)

  def async[R :_task, A](a: =>A): Eff[R, A] =
    send(Task.fork(Task.evalOnce(a)))

}

trait TaskInterpretation {

  def ApplicativeTask: Applicative[Task] = new Applicative[Task] {
    def pure[A](a: A): Task[A] =
      Task(a)

    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] =
      fa.zip(ff).map { case (a, f) => f(a) }
  }

  def awaitTask[R, U, A](r: Eff[R, A])(atMost: Duration)
      (implicit m: Member.Aux[Task, R, U], ec: ExecutionContext, s: Scheduler): Eff[U, Throwable Xor A] = {
    val recurse = new Recurse[Task, U, Throwable Xor A] {
      def apply[X](m: Task[X]) =
        try { Xor.left(Await.result(m.runAsync(s), atMost)) }
        catch { case NonFatal(t) => Xor.right(Eff.pure(Xor.left(t))) }

      def applicative[X, T[_]: Traverse](ms: T[Task[X]]): T[X] Xor Task[T[X]] =
        Xor.Right(ApplicativeTask.sequence(ms))
    }

    interpret1((a: A) => Xor.right(a): Throwable Xor A)(recurse)(r)
  }

}

object TaskInterpretation extends TaskInterpretation

