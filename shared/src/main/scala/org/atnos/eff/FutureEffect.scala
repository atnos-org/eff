package org.atnos.eff

import scala.util.control.NonFatal
import cats.data._
import Xor._
import Eff._
import Interpret._

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
  def sync[R, A](a: A)(implicit m: Member[Future, R]): Eff[R, A] =
    pure(a)

  def async[R, A](a: =>A)(implicit m: Member[Future, R], ec: ExecutionContext): Eff[R, A] =
    send(Future(a))

  def liftFuture[R, A](f: => Future[A])(implicit m: Member[Future, R], e: Member[EvalEffect.Eval, R]): Eff[R, A] =
    EvalEffect.delay(f).flatMap(v => Eff.send[Future, R, A](v))

}

trait FutureInterpretation {

  def awaitFuture[R <: Effects, U <: Effects, A](r: Eff[R, A])(atMost: Duration)
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

