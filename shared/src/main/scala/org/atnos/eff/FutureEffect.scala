package org.atnos.eff

import scala.util.control.NonFatal
import cats.data._
import Xor._
import Eff._
import Interpret._
import EvalTypes._
import scala.concurrent._
import duration._

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

}

trait FutureInterpretation {

  def awaitFuture[R, U, A](r: Eff[R, A])(atMost: Duration)
      (implicit m: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable Xor A] = {
    val recurse = new Recurse[Future, U, Throwable Xor A] {
      def apply[X](m: Future[X]) =
        try { Left(Await.result(m, atMost)) }
        catch { case NonFatal(t) => Right(Eff.pure(Left(t))) }
    }

    interpret1((a: A) => Right(a): Throwable Xor A)(recurse)(r)
  }

}

object FutureInterpretation extends FutureInterpretation

