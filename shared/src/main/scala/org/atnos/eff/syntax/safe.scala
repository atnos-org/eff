package org.atnos.eff.syntax


import org.atnos.eff._
import cats.data._

import scala.reflect.ClassTag

object safe extends safe

trait safe {

  implicit class SafeEffectOps[R, A](e: Eff[R, A]) {

    def runSafe[U](implicit m: Member.Aux[Safe, R, U]): Eff[U, (Throwable Xor A, List[Throwable])] =
      SafeEffect.runSafe[R, U, A](e)

    def execSafe[U](implicit m: Member.Aux[Safe, R, U]): Eff[U, Throwable Xor A] =
      SafeEffect.execSafe[R, U, A](e)

    def `finally`(last: Eff[R, Unit])(implicit m: Safe <= R): Eff[R, A] =
      SafeEffect.thenFinally(e, last)

    def thenFinally(last: Eff[R, Unit])(implicit m: Safe <= R): Eff[R, A] =
      SafeEffect.thenFinally(e, last)

    def catchThrowable[B](pure: A => B, onThrowable: Throwable => Eff[R, B])(implicit m: Safe <= R): Eff[R, B] =
      SafeEffect.catchThrowable(e, pure, onThrowable)

    def otherwise(onThrowable: Eff[R, A])(implicit m: Safe <= R): Eff[R, A] =
      SafeEffect.otherwise(e, onThrowable)

    def whenFailed(onThrowable: Throwable => Eff[R, A])(implicit m: Safe <= R): Eff[R, A] =
      SafeEffect.whenFailed(e, onThrowable)

    def attempt(implicit m: Safe <= R): Eff[R, Throwable Xor A] =
      SafeEffect.attempt(e)

    def ignoreException[E <: Throwable : ClassTag](implicit m: Safe <= R): Eff[R, Unit] =
      SafeEffect.ignoreException[R, E, A](e)
  }

}

