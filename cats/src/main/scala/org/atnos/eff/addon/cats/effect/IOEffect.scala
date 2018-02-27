package org.atnos.eff.addon.cats.effect

import cats.effect.{Async, IO}
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

  def runAsync[A](e: Eff[Fx1[IO], A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
    Eff.detach(e).runAsync(cb)

  def unsafeRunAsync[A](e: Eff[Fx1[IO], A])(cb: Either[Throwable, A] => Unit): Unit =
    Eff.detach(e).unsafeRunAsync(cb)

  def unsafeRunSync[A](e: Eff[Fx1[IO], A]): A =
    Eff.detach(e).unsafeRunSync

  def unsafeRunTimed[A](e: Eff[Fx1[IO], A], limit: Duration): Option[A] =
    Eff.detach(e).unsafeRunTimed(limit)

  def unsafeToFuture[A](e: Eff[Fx1[IO], A]): Future[A] =
    Eff.detach(e).unsafeToFuture

  def to[F[_], A](e: Eff[Fx1[IO], A])(implicit f: Async[F]): F[A] =
    Eff.detach[IO, Fx1[IO], A, Throwable](e).to[F]

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

trait IOInstances extends IOTypes { outer =>

  implicit def asyncInstance[R :_Io]: cats.effect.Async[Eff[R, ?]] = new cats.effect.Async[Eff[R, ?]] {

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): Eff[R, A] =
      fromIO(IO.async(k))

    def suspend[A](thunk: =>Eff[R, A]): Eff[R, A] =
      fromIO(IO.apply(thunk)).flatten

    def raiseError[A](e: Throwable): Eff[R, A] =
      fromIO(IO.raiseError(e))

    def handleErrorWith[A](fa: Eff[R, A])(f: Throwable => Eff[R, A]): Eff[R, A] =
      ioAttempt(fa).flatMap {
        case Left(t)  => f(t)
        case Right(a) => Eff.pure(a)
      }

    def pure[A](a: A): Eff[R,A] =
      Eff.pure(a)

    def flatMap[A, B](fa: Eff[R,A])(f: A =>Eff[R, B]): Eff[R, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Eff[R, Either[A, B]]): Eff[R, B] =
      Eff.EffMonad[R].tailRecM(a)(f)
  }


  def effectInstance[R :_Io](implicit runIO: Eff[R, Unit] => IO[Unit]): cats.effect.Effect[Eff[R, ?]] = new cats.effect.Effect[Eff[R, ?]] {
    private val asyncInstance = outer.asyncInstance

    def runAsync[A](fa: Eff[R, A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
      runIO(ioAttempt(fa).flatMap(r => fromIO(cb(r))))

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): Eff[R, A] =
      asyncInstance.async(k)

    def suspend[A](thunk: =>Eff[R, A]): Eff[R, A] =
      asyncInstance.suspend(thunk)

    def raiseError[A](e: Throwable): Eff[R, A] =
      asyncInstance.raiseError(e)

    def handleErrorWith[A](fa: Eff[R, A])(f: Throwable => Eff[R, A]): Eff[R, A] =
      asyncInstance.handleErrorWith(fa)(f)

    def pure[A](a: A): Eff[R,A] =
      Eff.pure(a)

    def flatMap[A, B](fa: Eff[R,A])(f: A =>Eff[R, B]): Eff[R, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Eff[R, Either[A, B]]): Eff[R, B] =
      Eff.EffMonad[R].tailRecM(a)(f)
  }
}

object IOInstances extends IOInstances

