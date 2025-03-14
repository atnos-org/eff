package org.atnos.eff.syntax.addon.cats

import cats.effect.IO
import cats.effect.LiftIO
import cats.effect.unsafe.IORuntime
import org.atnos.eff.*
import org.atnos.eff.addon.cats.effect.IOEffect
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

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
