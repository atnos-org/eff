package org.atnos.eff.syntax

import org.atnos.eff.*
import scala.reflect.ClassTag

object safe extends safe

trait safe {

  given safeExtension: AnyRef with {

    extension [R, A](e: Eff[R, A]) {

      def runSafe[U](using Member.Aux[Safe, R, U]): Eff[U, (Either[Throwable, A], List[Throwable])] =
        SafeEffect.runSafe[R, U, A](e)

      def execSafe[U](using Member.Aux[Safe, R, U]): Eff[U, Either[Throwable, A]] =
        SafeEffect.execSafe[R, U, A](e)

      def `finally`(last: Eff[R, Unit])(using Safe /= R): Eff[R, A] =
        SafeEffect.thenFinally(e, last)

      def thenFinally(last: Eff[R, Unit])(using Safe /= R): Eff[R, A] =
        SafeEffect.thenFinally(e, last)

      def catchThrowable[B](pure: A => B, onThrowable: Throwable => Eff[R, B])(using Safe /= R): Eff[R, B] =
        SafeEffect.catchThrowable(e, pure, onThrowable)

      def recoverThrowable[B](pure: A => B, onThrowable: PartialFunction[Throwable, Eff[R, B]])(using Safe /= R): Eff[R, B] =
        SafeEffect.recoverThrowable(e, pure, onThrowable)

      def otherwise(onThrowable: Eff[R, A])(using Safe /= R): Eff[R, A] =
        SafeEffect.otherwise(e, onThrowable)

      def whenFailed(onThrowable: Throwable => Eff[R, A])(using Safe /= R): Eff[R, A] =
        SafeEffect.whenFailed(e, onThrowable)

      def whenThrowable(onThrowable: PartialFunction[Throwable, Eff[R, A]])(using Safe /= R): Eff[R, A] =
        SafeEffect.whenThrowable(e, onThrowable)

      def attempt(using Safe /= R): Eff[R, Either[Throwable, A]] =
        SafeEffect.attempt(e)

      def ignoreException[E <: Throwable: ClassTag](using Safe /= R): Eff[R, Unit] =
        SafeEffect.ignoreException[R, E, A](e)
    }
  }

}
