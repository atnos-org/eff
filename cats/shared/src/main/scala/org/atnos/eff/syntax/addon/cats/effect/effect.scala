package org.atnos.eff.syntax.addon.cats

import cats.effect.{IO, LiftIO}
import cats.effect.unsafe.IORuntime
import org.atnos.eff._
import org.atnos.eff.addon.cats.effect.{IOEffect, IOInterpretation}

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import org.atnos.eff.Eff

package object effect {

  implicit final def toIOOps[A](e: Eff[Fx1[IO], A]): IOOps[A] = new IOOps[A](e)
  implicit final def toIOOps2[R, A](e: Eff[R, A]): IOOps2[R, A] = new IOOps2[R, A](e)

}

final class IOOps[A](private val e: Eff[Fx1[IO], A]) extends AnyVal {

  def unsafeRunAsync(cb: Either[Throwable, A] => Unit)(implicit i: IORuntime): Unit =
    IOEffect.unsafeRunAsync(e)(cb)

  def unsafeRunSync(implicit i: IORuntime): A =
    IOEffect.unsafeRunSync(e)

  def unsafeRunTimed(limit: FiniteDuration)(implicit i: IORuntime): Option[A] =
    IOEffect.unsafeRunTimed(e, limit)

  def unsafeToFuture(implicit i: IORuntime): Future[A] =
    IOEffect.unsafeToFuture(e)

  def to[F[_]](implicit f: LiftIO[F]): F[A] =
    IOEffect.to(e)

}

final class IOOps2[R, A](private val e: Eff[R, A]) extends AnyVal {

  def ioAttempt(implicit m: MemberInOut[IO, R]): Eff[R, Throwable Either A] =
    IOEffect.ioAttempt(e)

  def runIoMemo[U](cache: Cache)(implicit m: Member.Aux[Memoized, R, U], task: IO |= U): Eff[U, A] =
    IOEffect.runIoMemo(cache)(e)

  def ioMemo(key: AnyRef, cache: Cache)(implicit task: IO /= R): Eff[R, A] =
    IOInterpretation.ioMemo(key, cache, e)

}
