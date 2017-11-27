package org.atnos.eff.addon.cats.effect

import cats.effect.{Async, IO}
import IO._
import cats.~>
import org.atnos.eff._
import org.atnos.eff.syntax.eff._

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.Either
import IOEffect._

object IOEffect extends IOEffectCreation with IOInterpretation with IOInstances

trait IOTypes {

  type _io[R] = |=[IO, R]
  type _Io[R] = <=[IO, R]

}


trait IOEffectCreation extends IOTypes {

  final def fromIO[R :_io, A](io: IO[A]): Eff[R, A] =
    io.send[R]

  final def ioRaiseError[R :_io, A](t: Throwable): Eff[R, A] =
    IO.ioEffect.raiseError(t).send[R]

  final def ioDelay[R :_io, A](io: =>A): Eff[R, A] =
    IO.ioEffect.delay(io).send[R]

  final def ioFork[R :_io, A](io: =>A)(implicit ec: ExecutionContext): Eff[R, A] =
    ioShift >> ioDelay(io)

  final def ioSuspend[R :_io, A](io: =>IO[Eff[R, A]]): Eff[R, A] =
    IO.ioEffect.suspend(io).send[R].flatten

  final def ioShift[R :_io](implicit ec: ExecutionContext): Eff[R, Unit] =
    IO.ioEffect.shift(ec).send[R]

}

trait IOInterpretation extends IOTypes {

  def runAsync[R, A](e: Eff[R, A])(cb: Either[Throwable, A] => IO[Unit])(implicit m: Member.Aux[IO, R, NoFx]): IO[Unit] =
    Eff.detach(e).runAsync(cb)

  def unsafeRunAsync[R, A](e: Eff[R, A])(cb: Either[Throwable, A] => Unit)(implicit m: Member.Aux[IO, R, NoFx]): Unit =
    Eff.detach(e).unsafeRunAsync(cb)

  def unsafeRunSync[R, A](e: Eff[R, A])(implicit m: Member.Aux[IO, R, NoFx]): A =
    Eff.detach(e).unsafeRunSync

  def unsafeRunTimed[R, A](e: Eff[R, A], limit: Duration)(implicit m: Member.Aux[IO, R, NoFx]): Option[A] =
    Eff.detach(e).unsafeRunTimed(limit)

  def unsafeToFuture[R, A](e: Eff[R, A])(implicit m: Member.Aux[IO, R, NoFx]): Future[A] =
    Eff.detach(e).unsafeToFuture

  def to[R, F[_], A](e: Eff[R, A])(implicit f: Async[F], m: Member.Aux[IO, R, NoFx]): F[A] =
    Eff.detach[IO, R, A, Throwable](e).to[F]

  import interpret.of

  def ioAttempt[R, A](e: Eff[R, A])(implicit m: MemberInOut[IO, R]): Eff[R, Throwable Either A] = {
    import cats.instances.either._

    interpret.interceptNatM[R, IO, Throwable Either ?, A](e,
      new (IO ~> (IO of (Throwable Either ?))#l) {
        def apply[X](io: IO[X]): IO[Throwable Either X] =
          io.attempt
      })
  }
}

trait IOInstances extends IOTypes {
  implicit def effectInstance[R :_Io]: cats.effect.Effect[Eff[R, ?]] = new cats.effect.Effect[Eff[R, ?]] {
    def runAsync[A](fa: Eff[R, A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
      ioAttempt(fa).flatMap(r => fromIO(cb(r)))

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): Eff[R, A] =
      fromIO(IO.async(k))

    def suspend[A](thunk: =>Eff[R, A]): Eff[R, A] =
      fromIO(IO.suspend(thunk))

    def liftIO[A](ioa: IO[A]): Eff[R, A] =
      fromIO(ioa)

    def raiseError[A](e: Throwable): Eff[R, A] =
      fromIO(IO.raiseError(e))

    def handleErrorWith[A](fa: Eff[R, A])(f: Throwable => Eff[R, A]): Eff[R, A] =
      ioAttempt(fa).flatMap(f)
  }
}

object IOInstances extends IOInstances

