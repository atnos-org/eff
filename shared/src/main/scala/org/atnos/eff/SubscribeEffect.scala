package org.atnos.eff

import cats._

import scala.concurrent._
import duration._
import Eff._
import org.atnos.eff.either._
import org.atnos.eff.syntax.either._

import scala.util.control.NonFatal

/**
 * This effect is used in the implementation of the Async effect
 */
object SubscribeEffect {

  type Callback[A] = (Throwable Either A) => Unit

  trait Subscribe[A] extends (Callback[A] => Unit)

  case class SimpleSubscribe[A](subscribe: Callback[A] => Unit) extends Subscribe[A] {
    def apply(cb: Callback[A]): Unit = subscribe(cb)
  }

  case class AttemptedSubscribe[A](subscribe: Callback[Throwable Either A] => Unit) extends Subscribe[Throwable Either A] {
    def apply(cb: Callback[Throwable Either A]): Unit = subscribe(cb)
  }

  type _subscribe[R] = Subscribe |= R

  type FS = Fx.fx1[Subscribe]

  def subscribeToAttemptedSubscribe = new (Subscribe ~> AttemptedSubscribe) {

    def apply[X](subscribe: Subscribe[X]): AttemptedSubscribe[X] =
      AttemptedSubscribe((c: Callback[Throwable Either X]) => subscribe((tx: Throwable Either X) => c(Right(tx))))
  }

  def subscribeAttempt[A](e: Eff[FS, A])(implicit m: Subscribe /= FS): Eff[FS, ThrowableEither[A]] = {
    type U = Fx.prepend[ThrowableEither, FS]

    interpret.translateInto[FS, Subscribe, U, A](e)(new Translate[Subscribe, U] {
      def apply[X](sx: Subscribe[X]): Eff[U, X] = {

        send[Subscribe, U, ThrowableEither[X]](AttemptedSubscribe((c: Callback[Throwable Either X]) => {

          sx.apply((tx: Throwable Either X) => try {

            c(Right(tx))} catch { case NonFatal(t) => c(Right(Left(t))) })})).
            flatMap {
              case Left(t)  => left[U, Throwable, X](t)
              case Right(x) => right[U, Throwable, X](x)
            }
      }
    }).runEither
  }

}

