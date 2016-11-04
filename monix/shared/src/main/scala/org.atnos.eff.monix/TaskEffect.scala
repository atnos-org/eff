package org.atnos.eff.monix

import scala.util.control.NonFatal
import cats._
import cats.data._
import cats.implicits._
import org.atnos.eff._
import either._
import org.atnos.eff.syntax.all._
import Eff._
import Interpret._
import _root_.monix.eval._
import _root_.monix.execution._

import scala.concurrent._
import duration._
import TaskEffect._

import scala.util.{Failure, Success}

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

  def suspend[R :_task, A](task: => Task[Eff[R, A]]): Eff[R, A] =
    send[Task, R, Eff[R, A]](Task.suspend(task)).flatten

}

trait TaskInterpretation {

  implicit def ApplicativeMonad: Monad[Task] = new Monad[Task] {
    def pure[A](a: A): Task[A] =
      Task(a)

    def flatMap[A, B](fa: Task[A])(f: A => Task[B]): Task[B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Task[Either[A, B]]): Task[B] =
      flatMap(f(a)) {
        case Right(b)   => pure(b)
        case Left(next) => tailRecM(next)(f)
      }
  }

  def ApplicativeTask: Applicative[Task] = new Applicative[Task] {
    def pure[A](a: A): Task[A] =
      Task(a)

    def ap[A, B](ff: Task[A => B])(fa: Task[A]): Task[B] =
      fa.zip(ff).map { case (a, f) => f(a) }
  }

  def awaitTask[R, U, A](r: Eff[R, A])(atMost: Duration)
      (implicit m: Member.Aux[Task, R, U], ec: ExecutionContext, s: Scheduler): Eff[U, Throwable Either A] = {
    val recurse = new Recurse[Task, U, Throwable Either A] {
      def apply[X](m: Task[X]) =
        try { Left(Await.result(m.runAsync(s), atMost)) }
        catch { case NonFatal(t) => Right(Eff.pure(Left(t))) }

      def applicative[X, T[_]: Traverse](ms: T[Task[X]]): T[X] Either Task[T[X]] =
        Right(ApplicativeTask.sequence(ms))
    }

    interpret1((a: A) => Right(a): Throwable Either A)(recurse)(r)
  }

  def attempt[R, A](e: Eff[R, A])(implicit task: Task /= R): Eff[R, Throwable Either A] = {
    e match {
      case Pure(a) => pure[R, Throwable Either A](Right(a))

      case Impure(u, c) =>
        task.extract(u) match {
          case Some(tx) =>
            val union = task.inject(tx.materialize.map {
              case Success(x) => Right[Throwable, u.X](x): Either[Throwable, u.X]
              case Failure(t) => Left[Throwable, u.X](t): Either[Throwable, u.X]
            })
            Impure(union, Arrs.singleton { ex: (Throwable Either u.X) =>
              ex match {
                case Right(x) => attempt(c(x))
                case Left(t)  => pure(Left(t): Either[Throwable, A])
              }
            })

          case None => Impure(u, Arrs.singleton((x: u.X) => attempt(c(x))))
        }

        case ImpureAp(unions, c) =>
          def materialize(u: Union[R, Any]): Union[R, Any] =
            task.extract(u) match {
              case Some(tx) => task.inject(tx.materialize.map {
                case Success(x) => Right(x)
                case Failure(t) => Left(t)
              })
              case None => u
            }

          val materializedUnions =
            Unions(materialize(unions.first), unions.rest.map(materialize))

          val collected = unions.extract(task)
          val continuation = Arrs.singleton[R, List[Any], Throwable Either A] { ls: List[Any] =>
            val eithers =
              ls.zipWithIndex.collect { case (a, i) =>
                if (collected.indices.contains(i)) a.asInstanceOf[Throwable Either Any]
                else Right(a)
              }.sequence

            eithers match {
              case Left(t) => pure(Left(t))
              case Right(anys) => attempt(c(anys))
            }
          }

          ImpureAp(materializedUnions, continuation)
    }
  }

  def withTimeout[R, A](e: Eff[R, A])(duration: FiniteDuration)(implicit task: Task /= R): Eff[R, A] =
    interceptNat[R, Task, A](e)(new (Task ~> Task) { def apply[X](t: Task[X]) = t.timeout(duration) })

}

object TaskInterpretation extends TaskInterpretation

