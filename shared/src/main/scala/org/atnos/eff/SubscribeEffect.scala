package org.atnos.eff

import cats._

import scala.concurrent._, duration._
import Eff._
import org.atnos.eff.either._
import org.atnos.eff.syntax.either._

/**
 * This effect is used in the implementation of the Async effect
 */
object SubscribeEffect {

  type Callback[A] = (Throwable Either A) => Unit

  type Subscribe[A] = Callback[A] => Unit
  type AttemptedSubscribe[A] = Callback[Throwable Either A] => Unit

  type _subscribe[R] = Subscribe |= R

  type FS = Fx.fx1[Subscribe]

  def subscribeToAttemptedSubscribe = new (Subscribe ~> AttemptedSubscribe) {
    def apply[X](subscribe: Subscribe[X]): AttemptedSubscribe[X] =
      (c: Callback[Throwable Either X]) => subscribe((tx: Throwable Either X) => c(Right(tx)))
  }

  def subscribeAttempt[A](e: Eff[FS, A])(implicit m: Subscribe /= FS): Eff[FS, ThrowableEither[A]] = {
    type U = Fx.prepend[ThrowableEither, FS]

    interpret.translateInto[FS, Subscribe, U, A](e)(new Translate[Subscribe, U] {
      def apply[X](sx: Subscribe[X]): Eff[U, X] =
        send[Subscribe, U, ThrowableEither[X]]((c: Callback[Throwable Either X]) => sx.apply((tx: Throwable Either X) => c(Right(tx)))).flatMap {
          case Left(t)  => left[U, Throwable, X](t)
          case Right(x) => right[U, Throwable, X](x)
        }
    }).runEither
  }

}

