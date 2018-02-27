package org.atnos.eff.syntax.addon.cats

import cats.effect.{Async, IO}
import org.atnos.eff._
import org.atnos.eff.addon.cats.effect.IOEffect

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration
import org.atnos.eff.Eff

object effect {

  implicit final def toIOOps[A](e: Eff[Fx1[IO], A]): IOOps[A] = new IOOps[A](e)
  implicit final def toIOOps2[R, A](e: Eff[R, A]): IOOps2[R, A] = new IOOps2[R, A](e)

}

final class IOOps[A](val e: Eff[Fx1[IO], A]) extends AnyVal {

  def runAsync(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
    IOEffect.runAsync(e)(cb)

  def unsafeRunAsync(cb: Either[Throwable, A] => Unit): Unit =
    IOEffect.unsafeRunAsync(e)(cb)

  def unsafeRunSync: A =
    IOEffect.unsafeRunSync(e)

  def unsafeRunTimed(limit: Duration): Option[A] =
    IOEffect.unsafeRunTimed(e, limit)

  def unsafeToFuture: Future[A] =
    IOEffect.unsafeToFuture(e)

  def to[F[_]](implicit f: Async[F]): F[A] =
    IOEffect.to(e)

}

final class IOOps2[R, A](val e: Eff[R, A]) extends AnyVal {

  def ioAttempt(implicit m: MemberInOut[IO, R]): Eff[R, Throwable Either A] =
    IOEffect.ioAttempt(e)

  def ioShift(implicit m: MemberIn[IO, R], ec: ExecutionContext): Eff[R, A] =
    IOEffect.ioShift >> e
}
