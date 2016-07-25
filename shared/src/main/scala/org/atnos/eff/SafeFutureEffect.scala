package org.atnos.eff

import scala.util.control.NonFatal
import cats._, data._
import Xor._
import Eff._
import Interpret._
import scala.concurrent._
import duration._
import cats.implicits._
import SafeFutureEffect._

/**
 * Effect for Future computations
 */
trait SafeFutureEffect extends
  SafeFutureCreation with
  SafeFutureInterpretation  {

  type _SafeFuture[R] = SafeFuture <= R
  type _safeFuture[R] = SafeFuture |= R

}

object SafeFutureEffect extends SafeFutureEffect

trait SafeFutureCreation {

  def safeAsync[R :_safeFuture, A](a: =>A)(implicit ec: ExecutionContext): Eff[R, A] =
    send(SafeFuture(() => Future(a).attempt))

  def safeLift[R :_safeFuture, A](fa: =>Future[A])(implicit ec: ExecutionContext): Eff[R, A] =
    send(SafeFuture(() => fa.attempt))

  def safeAttempt[R :_safeFuture, A](fa: =>Future[A])(implicit ec: ExecutionContext): Eff[R, Throwable Xor A] =
    send(SafeFuture(() => fa.attempt.map(Xor.right)))
}

object SafeFutureCreation extends SafeFutureCreation

trait SafeFutureInterpretation {

  def awaitSafeFuture[R, U, A](r: Eff[R, A])(atMost: Duration)
      (implicit m: Member.Aux[SafeFuture, R, U], ec: ExecutionContext): Eff[U, Throwable Xor A] = {
    val recurse = new Recurse[SafeFuture, U, Throwable Xor A] {
      def apply[X](fx: SafeFuture[X]) =
        try { Await.result(fx.run(), atMost).fold(t => Right(Eff.pure(Left(t))), x => Left(x)) }
        catch { case NonFatal(t) => Right(Eff.pure(Left(t))) }
    }

    interpret1((a: A) => Right(a): Throwable Xor A)(recurse)(r)
  }

}

object SafeFutureInterpretation extends SafeFutureInterpretation

case class SafeFuture[A](run: () => Future[Throwable Xor A]) {
  def map[B](f: A => B)(implicit ec: ExecutionContext): SafeFuture[B] =
    SafeFuture(() => run().map(_.map(f)))

  def flatMap[B](f: A => SafeFuture[B])(implicit ec: ExecutionContext): SafeFuture[B] =
    SafeFuture(() => run().flatMap(xor => xor.fold(t => Future.successful(Xor.Left(t)), a => f(a).run())))

  lazy val value: Future[Throwable Xor A] =
    run()
}

object SafeFuture {
  implicit def MonadSafeFuture(implicit ec: ExecutionContext): Monad[SafeFuture] = new Monad[SafeFuture] {
    def pure[A](a: A): SafeFuture[A] =
      SafeFuture(() => Future.successful(Xor.right(a)))

    def flatMap[A, B](fa: SafeFuture[A])(f: A => SafeFuture[B]): SafeFuture[B] =
      fa.flatMap(f)
  }
}
