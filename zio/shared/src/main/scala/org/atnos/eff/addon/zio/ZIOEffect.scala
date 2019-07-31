package org.atnos.eff.addon.zio


import cats._
import cats.instances.either._
import org.atnos.eff._
import org.atnos.eff.create.send
import zio.{IO, Task, UIO, ZIO}

import scala.util.Either


trait ZIOTypes {
  type _uio[R] = |=[UIO, R]
  type _UIO[R] = <=[UIO, R]

  type _task[R] = |=[Task, R]
  type _Task[R] = <=[Task, R]
}

object ZIOTypes extends ZIOTypes

trait ZIOCreation extends ZIOTypes {
  final def fromZIO[R, ENV, E, A](zio: ZIO[ENV, E, A])(implicit member: ZIO[ENV, E, ?] |= R): Eff[R, A] =
    send[ZIO[ENV, E, ?], R, A](zio)

  final def succeedLazy[R, A](a: => A)(implicit member: UIO |= R): Eff[R, A] =
    fromZIO(ZIO.succeedLazy(a))

  final def task[R, A](a: => A)(implicit member: Task |= R): Eff[R, A] =
    fromZIO(ZIO.effect(a))
}

object ZIOCreation extends ZIOCreation

trait ZIOInterpretation {
  private def zioMonad[R, E]: MonadError[ZIO[R, E, ?], E] = new MonadError[ZIO[R, E, ?], E] {
    override final def flatMap[A, B](fa: ZIO[R, E, A])(f: A => ZIO[R, E, B]): ZIO[R, E, B] = fa.flatMap(f)

    override final def raiseError[A](e: E): IO[E, A] = ZIO.fail(e)

    override final def handleErrorWith[A](fa: ZIO[R, E, A])(f: E => ZIO[R, E, A]): ZIO[R, E, A] = fa.catchAll(f)

    override final def pure[A](x: A): ZIO[R, E, A] = ZIO.succeed(x)

    override final def tailRecM[A, B](a: A)(f: A => ZIO[R, E, Either[A, B]]): ZIO[R, E, B] = ZIO.suspend(f(a)).flatMap {
      case Left(l)  => tailRecM(l)(f)
      case Right(r) => ZIO.succeed(r)
    }
  }

  private def zioApplicative[R, E]: CommutativeApplicative[ZIO[R, E, ?]] = new CommutativeApplicative[ZIO[R, E, ?]] {
    final override def pure[A](x: A): ZIO[R, E, A] =
      ZIO.succeed(x)

    final override def map2[A, B, Z](fa: ZIO[R, E, A], fb: ZIO[R, E, B])(f: (A, B) => Z): ZIO[R, E, Z] =
      fa.zipPar(fb).map(f.tupled)

    final override def ap[A, B](ff: ZIO[R, E, A => B])(fa: ZIO[R, E, A]): ZIO[R, E, B] =
      ff.flatMap(fa.map)

    final override def product[A, B](fa: ZIO[R, E, A], fb: ZIO[R, E, B]): ZIO[R, E, (A, B)] =
      map2(fa, fb)(_ -> _)

    final override def map[A, B](fa: ZIO[R, E, A])(f: A => B): ZIO[R, E, B] =
      fa.map(f)

    final override def unit: ZIO[R, E, Unit] =
      ZIO.unit
  }

  def runAsync[R, ENV, E, A](e: Eff[R, A])(implicit m: Member.Aux[ZIO[ENV, E, ?], R, NoFx]): ZIO[ENV, E, A] =
    Eff.detachA[ZIO[ENV, E, ?], R, A, E](e)(zioMonad, zioApplicative, m)

  def runSequential[R, ENV, E, A](e: Eff[R, A])(implicit m: Member.Aux[ZIO[ENV, E, ?], R, NoFx]): ZIO[ENV, E, A] =
    Eff.detach[ZIO[ENV, E, ?], R, A, E](e)(zioMonad, m)

  import interpret.of

  def either[R, ENV, E, A](e: Eff[R, A])(implicit zio: ZIO[ENV, E, ?] /= R): Eff[R, E Either A] =
    interpret.interceptNatM[R, ZIO[ENV, E, ?], E Either ?, A](e,
      new (ZIO[ENV, E, ?] ~> (ZIO[ENV, E, ?] of (E Either ?))#l) {
        def apply[X](fa: ZIO[ENV, E, X]): ZIO[ENV, E, E Either X] =
          fa.either
      })
}

object ZIOInterpretation extends ZIOInterpretation

trait ZIOEffect extends ZIOCreation with ZIOInterpretation

object ZIOEffect extends ZIOEffect