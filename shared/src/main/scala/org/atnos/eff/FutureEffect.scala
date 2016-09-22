package org.atnos.eff

import scala.util.control.NonFatal
import cats.data._
import cats.{Applicative, Traverse}
import cats.implicits._
import Xor._
import Eff._
import Interpret._
import EvalTypes._

import scala.concurrent._
import duration._
import XorCreation._

/**
 * Effect for Future computations
 */
trait FutureEffect extends
  FutureCreation with
  FutureInterpretation

object FutureEffect extends FutureEffect

trait FutureCreation {

  type _Future[R] = Future <= R
  type _future[R] = Future |= R

  def sync[R :_future, A](a: A): Eff[R, A] =
    pure(a)

  def async[R :_future, A](a: =>A)(implicit ec: ExecutionContext): Eff[R, A] =
    send(Future(a))

  def liftFuture[R :_future :_eval, A](f: =>Future[A]): Eff[R, A] =
    EvalEffect.delay(f).flatMap(v => Eff.send[Future, R, A](v))

  def attemptFuture[R :_future :_eval :_throwableXor, A](f: =>Future[A])(implicit ec: ExecutionContext): Eff[R, A] =
    liftFuture(f.attempt).flatMap(send(_))

}

trait FutureInterpretation {

  def ApplicativeFuture(implicit ec: ExecutionContext): Applicative[Future] = new Applicative[Future] {
    def pure[A](x: A): Future[A] =
      Future.successful(x)

    def ap[A, B](ff: Future[A => B])(fa: Future[A]): Future[B] =
      fa.zip(ff).map { case (a, f) => f(a) }
  }

  def awaitFuture[R, U, A](r: Eff[R, A])(atMost: Duration)
      (implicit m: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable Xor A] = {
    val recurse = new Recurse[Future, U, Throwable Xor A] {
      def apply[X](m: Future[X]) =
        try { Left(Await.result(m, atMost)) }
        catch { case NonFatal(t) => Right(Eff.pure(Left(t))) }

      def applicative[X, T[_]: Traverse](ms: T[Future[X]]): T[X] Xor Future[T[X]] =
        Xor.Right(ApplicativeFuture.sequence(ms))
    }

    interpret1((a: A) => Right(a): Throwable Xor A)(recurse)(r)
  }

}

object FutureInterpretation extends FutureInterpretation

