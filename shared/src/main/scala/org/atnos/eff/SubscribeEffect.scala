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

  trait Subscribe[A] extends (Callback[A] => Unit) {
    def memoizeKey: Option[(AnyRef, Cache)]
    def unmemoize: Subscribe[A]
  }

  case class SimpleSubscribe[A](subscribe: Callback[A] => Unit, memoizeKey: Option[(AnyRef, Cache)] = None) extends Subscribe[A] {
    def apply(cb: Callback[A]): Unit = subscribe(cb)

    def unmemoize: Subscribe[A] =
      copy(memoizeKey = None)

    override def toString: String =
      s"SimpleSubscribe(<subscribe>, $memoizeKey)"
  }

  case class AttemptedSubscribe[A](subscribe: Callback[Throwable Either A] => Unit, memoizeKey: Option[(AnyRef, Cache)] = None) extends Subscribe[Throwable Either A] {
    def apply(cb: Callback[Throwable Either A]): Unit = subscribe(cb)

    def unmemoize: AttemptedSubscribe[A] =
      copy(memoizeKey = None)

    override def toString: String =
      s"AttemptedSubscribe(<subscribe>, $memoizeKey)"
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

            c(Right(tx))} catch { case NonFatal(t) => c(Right(Left(t))) })}, sx.memoizeKey)).
            flatMap {
              case Left(t)  => left[U, Throwable, X](t)
              case Right(x) => right[U, Throwable, X](x)
            }
      }
    }).runEither
  }

  def memoizeSubscribe[A](key: AnyRef, cache: Cache, e: Subscribe[A]): Subscribe[A] =
    e match {
      case SimpleSubscribe(s, _)    => SimpleSubscribe(s, Option((key, cache)))
      case AttemptedSubscribe(s, _) => AttemptedSubscribe(s, Option((key, cache)))
    }

  def memoize[K <: AnyRef, A](key: K, sequenceKey: Int, cache: Cache, e: Eff[FS, A]): Eff[FS, A] = {
    memoizeSubsequence(key, sequenceKey, 0, cache, e)
  }

  private def memoizeSubsequence[K <: AnyRef, A](key: K, sequenceKey: Int, subSequenceKey: Int, cache: Cache, e: Eff[FS, A]): Eff[FS, A] = {
    var sub = subSequenceKey

    def cacheKey =
      key.toString+"-"+sequenceKey+"-"+sub

    def materialize(u: Union[FS, Any]): Union[FS, Any] =
      u match { case Union1(fs) =>
        val u1 = Union1(memoizeSubscribe(cacheKey, cache, fs))
        sub += 1
        u1
      }

    e match {
      case Pure(a, last) =>
        Pure(a, last)

      case Impure(u, c, last) =>
        Impure(materialize(u), Arrs.singleton((x: u.X) => memoizeSubsequence(key, sequenceKey, sub, cache, c(x))), last)

      case ImpureAp(unions, continuation, last) =>

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val continuation1 = Arrs.singleton[FS, Vector[Any], A]((ls: Vector[Any]) => memoizeSubsequence(key, sequenceKey, sub, cache, continuation(ls)))
        ImpureAp(materializedUnions, continuation1, last)
    }
  }
}

