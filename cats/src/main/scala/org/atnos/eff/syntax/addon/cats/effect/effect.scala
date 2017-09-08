package org.atnos.eff.syntax.addon.cats

import cats.effect.{Async, IO}
import org.atnos.eff._
import org.atnos.eff.addon.cats.effect.IOEffect

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration
import org.atnos.eff.Eff

object effect {

  implicit final def toIOOps[R, A](e: Eff[R, A]): IOOps[R, A] = new IOOps[R, A](e)

}

final class IOOps[R, A](val e: Eff[R, A]) extends AnyVal {

  def runAsync(cb: Either[Throwable, A] => IO[Unit])(implicit m: Member.Aux[IO, R, NoFx]): IO[Unit] =
    IOEffect.runAsync(e)(cb)

  def unsafeRunAsync(cb: Either[Throwable, A] => Unit)(implicit m: Member.Aux[IO, R, NoFx]): Unit =
    IOEffect.unsafeRunAsync(e)(cb)

  def unsafeRunSync(implicit m: Member.Aux[IO, R, NoFx]): A =
    IOEffect.unsafeRunSync(e)

  def unsafeRunTimed(limit: Duration)(implicit m: Member.Aux[IO, R, NoFx]): Option[A] =
    IOEffect.unsafeRunTimed(e, limit)

  def unsafeToFuture(implicit m: Member.Aux[IO, R, NoFx]): Future[A] =
    IOEffect.unsafeToFuture(e)

  def to[F[_]](implicit f: Async[F], m: Member.Aux[IO, R, NoFx]): F[A] =
    IOEffect.to(e)

  def ioAttempt(implicit m: MemberInOut[IO, R]): Eff[R, Throwable Either A] =
    IOEffect.ioAttempt(e)

  def ioShift(implicit m: MemberIn[IO, R], ec: ExecutionContext): Eff[R, A] =
    IOEffect.ioShift >> e
}
