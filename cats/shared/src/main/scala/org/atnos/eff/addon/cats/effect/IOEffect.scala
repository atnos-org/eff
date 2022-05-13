package org.atnos.eff.addon.cats.effect

import cats.effect.IO
import cats.effect.LiftIO
import cats.effect.unsafe.IORuntime
import cats.~>
import org.atnos.eff._
import org.atnos.eff.syntax.eff._
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.Either

object IOEffect extends IOEffectCreation with IOInterpretation

trait IOTypes {

  type _io[R] = |=[IO, R]
  type _Io[R] = <=[IO, R]

}

trait IOEffectCreation extends IOTypes {

  final def fromIO[R: _io, A](io: IO[A]): Eff[R, A] =
    io.send[R]

  final def ioRaiseError[R: _io, A](t: Throwable): Eff[R, A] =
    IO.raiseError(t).send[R]

  final def ioDelay[R: _io, A](io: => A): Eff[R, A] =
    IO(io).send[R]

  final def ioSuspend[R: _io, A](io: => IO[Eff[R, A]]): Eff[R, A] =
    IO.defer(io).send[R].flatten

}

object IOInterpretation extends IOInterpretation

trait IOInterpretation extends IOTypes {

  def unsafeRunAsync[A](e: Eff[Fx1[IO], A])(cb: Either[Throwable, A] => Unit)(implicit i: IORuntime): Unit =
    Eff.detach(e).unsafeRunAsync(cb)

  def unsafeRunSync[A](e: Eff[Fx1[IO], A])(implicit i: IORuntime): A =
    Eff.detach(e).unsafeRunSync()

  def unsafeRunTimed[A](e: Eff[Fx1[IO], A], limit: FiniteDuration)(implicit i: IORuntime): Option[A] =
    Eff.detach(e).unsafeRunTimed(limit)

  def unsafeToFuture[A](e: Eff[Fx1[IO], A])(implicit i: IORuntime): Future[A] =
    Eff.detach(e).unsafeToFuture()

  def to[F[_], A](e: Eff[Fx1[IO], A])(implicit f: LiftIO[F]): F[A] =
    Eff.detach[IO, Fx1[IO], A, Throwable](e).to[F]

  import interpret.of

  def ioAttempt[R, A](e: Eff[R, A])(implicit m: MemberInOut[IO, R]): Eff[R, Throwable Either A] = {

    interpret.interceptNatM[R, IO, Either[Throwable, *], A](
      e,
      new IO ~> (IO of Either[Throwable, *])#l {
        def apply[X](io: IO[X]): IO[Throwable Either X] =
          io.attempt
      }
    )
  }

  /** memoize the io result using a cache */
  def memoize[A](key: AnyRef, cache: Cache, io: IO[A]): IO[A] =
    cache.get[A](key).fold(io.map { r => cache.put(key, r); r })(IO.pure)

  /**
    * Memoize io effects using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  def ioMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit task: IO /= R): Eff[R, A] =
    ioAttempt(Eff.memoizeEffect(e, cache, key)).flatMap {
      case Left(t) => Eff.send(ioSequenceCached.reset(cache, key)) >> IOEffect.ioRaiseError(t)
      case Right(a) => Eff.pure(a)
    }

  /**
    * Memoize task values using a memoization effect
    *
    * if this method is called with the same key the previous value will be returned
    */
  def ioMemoized[R, A](key: AnyRef, e: Eff[R, A])(implicit task: IO /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R].flatMap(cache => ioMemo(key, cache, e))

  def runIoMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], task: IO |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => IOEffect.ioDelay(cache.memo(key, value()))
          case GetCache() => IOEffect.ioDelay(cache)
        }
    })
  }

  implicit val ioSequenceCached: SequenceCached[IO] = new SequenceCached[IO] {
    def get[X](cache: Cache, key: AnyRef): IO[Option[X]] =
      IO(cache.get(key))

    def apply[X](cache: Cache, key: AnyRef, sequenceKey: Int, tx: => IO[X]): IO[X] =
      cache.memo((key, sequenceKey), tx)

    def reset(cache: Cache, key: AnyRef): IO[Unit] =
      IO {
        cache.reset(key)
        var i = 0
        while (cache.get((key, i)).isDefined) {
          cache.reset((key, i))
          i += 1
        }
      }
  }

}
