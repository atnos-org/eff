package org.atnos.eff

import cats.*
import org.atnos.eff.Eff.send
import org.atnos.eff.either.*
import org.atnos.eff.syntax.all.given
import scala.util.control.NonFatal

/**
 * This effect is used in the implementation of the Async effect
 */
object SubscribeEffect {

  type Callback[A] = (Either[Throwable, A]) => Unit

  sealed abstract class Subscribe[A] extends (Callback[A] => Unit) {
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

  case class AttemptedSubscribe[A](subscribe: Callback[Either[Throwable, A]] => Unit, memoizeKey: Option[(AnyRef, Cache)] = None)
      extends Subscribe[Either[Throwable, A]] {
    def apply(cb: Callback[Either[Throwable, A]]): Unit = subscribe(cb)

    def unmemoize: AttemptedSubscribe[A] =
      copy(memoizeKey = None)

    override def toString: String =
      s"AttemptedSubscribe(<subscribe>, $memoizeKey)"
  }

  type _subscribe[R] = Subscribe |= R

  type FS = Fx.fx1[Subscribe]

  def subscribeToAttemptedSubscribe: Subscribe ~> AttemptedSubscribe = new (Subscribe ~> AttemptedSubscribe) {

    def apply[X](subscribe: Subscribe[X]): AttemptedSubscribe[X] =
      AttemptedSubscribe((c: Callback[Either[Throwable, X]]) => subscribe((tx: Either[Throwable, X]) => c(Right(tx))))
  }

  def subscribeAttempt[A](e: Eff[FS, A]): Eff[FS, ThrowableEither[A]] = {
    type U = Fx.prepend[ThrowableEither, FS]

    interpret
      .translateInto[FS, Subscribe, U, A](e)(new Translate[Subscribe, U] {
        def apply[X](sx: Subscribe[X]): Eff[U, X] = {

          send[Subscribe, U, ThrowableEither[X]](
            AttemptedSubscribe(
              (c: Callback[Either[Throwable, X]]) => {
                sx.apply((tx: Either[Throwable, X]) =>
                  try {
                    c(Right(tx))
                  } catch {
                    case NonFatal(t) => c(Right(Left(t)))
                  }
                )
              },
              sx.memoizeKey
            )
          ).flatMap {
            case Left(t) => left[U, Throwable, X](t)
            case Right(x) => right[U, Throwable, X](x)
          }
        }
      })
      .runEither[Throwable]
      .into[FS]
  }

  def memoizeSubscribe[A](key: AnyRef, cache: Cache, e: Subscribe[A]): Subscribe[A] =
    e match {
      case SimpleSubscribe(s, _) => SimpleSubscribe(s, Option((key, cache)))
      case AttemptedSubscribe(s, _) => AttemptedSubscribe(s, Option((key, cache)))
    }

  def memoize[K <: AnyRef, A](key: K, sequenceKey: Int, cache: Cache, e: Eff[FS, A]): Eff[FS, A] = {
    memoizeSubsequence(key, sequenceKey, 0, cache, e)
  }

  private def memoizeSubsequence[K <: AnyRef, A](key: K, sequenceKey: Int, subSequenceKey: Int, cache: Cache, e: Eff[FS, A]): Eff[FS, A] = {
    var sub = subSequenceKey

    def cacheKey =
      s"${key}-${sequenceKey}-${sub}"

    def materialize(u: Union[FS, Any]): Union[FS, Any] = {
      val tagged = u.tagged
      val u1 = tagged.copy(valueUnsafe = memoizeSubscribe(cacheKey, cache, tagged.valueUnsafe.asInstanceOf[Subscribe[Any]]))
      sub += 1
      u1.forget
    }

    e match {
      case x @ Pure(_, _) =>
        x

      case Impure(NoEffect(a), c, last) =>
        memoizeSubsequence(key, sequenceKey, subSequenceKey, cache, c(a).addLast(last))

      case Impure(u: Union[?, ?], c, last) =>
        Impure(
          materialize(u.asInstanceOf[Union[FS, Any]]),
          c.mapLast(r => memoizeSubsequence(key, sequenceKey, sub, cache, r)).asInstanceOf[Continuation[Fx1[Subscribe], Any, A]],
          last
        )

      case ImpureAp(unions, continuation, last) =>
        val materializedUnions =
          Unions(materialize(unions.first.asInstanceOf[Union[FS, Any]]), unions.rest.map(materialize))

        val continuation1 = continuation.mapLast(r => memoizeSubsequence(key, sequenceKey, sub, cache, r))
        ImpureAp(materializedUnions, continuation1, last)
    }
  }
}
