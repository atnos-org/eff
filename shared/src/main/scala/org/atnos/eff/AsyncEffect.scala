package org.atnos.eff

import java.util.UUID

import cats._
import cats.implicits._
import org.atnos.eff.all._

import scala.concurrent.duration.FiniteDuration
import SubscribeEffect._

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
trait AsyncEffect extends AsyncCreation

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
object AsyncEffect extends AsyncEffect

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
trait AsyncCreation {

  type _async[R] = Async |= R
  type _Async[R] = Async <= R

  def subscribe[R :_async, A](c: Subscribe[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[Async, R, A](AsyncEff(send[Subscribe, FS, A](c), timeout))

  def asyncNow[R :_async, A](a: A): Eff[R, A] =
    send[Async, R, A](AsyncNow[A](a))

  def asyncFail[R :_async, A](t: Throwable): Eff[R, A] =
    send[Async, R, A](AsyncFailed[A](t))

  def asyncFromEither[R :_async, A](e: Throwable Either A): Eff[R, A] =
    e.fold(t => asyncFail(t), a => asyncNow(a))

  def asyncDelay[R :_async, A](a: =>A): Eff[R, A] =
    send[Async, R, A](AsyncDelayed[A](Eval.later(a)))

  def asyncFork[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[Async, R, A](AsyncEff(send[Subscribe, FS, A](SimpleSubscribe((c: Callback[A]) => c(Either.catchNonFatal(a)))), timeout))

  def fork[R :_async, A](a: =>Async[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    asyncDelay[R, Eff[R, A]] {
      a match {
        case AsyncNow(a1)     => asyncFork(a1, timeout)
        case AsyncDelayed(a1) => asyncFork(a1.value, timeout)
        case AsyncFailed(t)   => asyncFail(t)
        case AsyncEff(e, to)  => send[Async, R, A](AsyncEff(e, to))
      }
    }.flatten

  def async[R :_async, A](subscribe: Subscribe[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[Async, R, A](AsyncEff(send[Subscribe, FS, A](subscribe), timeout))

}

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
object AsyncCreation extends AsyncCreation

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
trait AsyncInterpretation {

  import interpret.of

  def asyncAttempt[R, A](e: Eff[R, A])(implicit async: Async /= R): Eff[R, Throwable Either A] =
    interpret.interceptNatM(e, new (Async ~> (Async of (Throwable Either ?))#l) {
      override def apply[X](fa: Async[X]): Async[Throwable Either X] = attempt(fa)
    })

  def attempt[A](a: Async[A]): Async[Throwable Either A] =
    a match {
      case AsyncNow(a1)     => AsyncNow[Throwable Either A](Right(a1))
      case AsyncFailed(t)   => AsyncNow[Throwable Either A](Left(t))
      case AsyncDelayed(a1) => AsyncDelayed(Eval.later(Either.catchNonFatal(a1.value)))
      case AsyncEff(e, to)  => AsyncEff(subscribeAttempt(e), to)
    }

  implicit final def toAsyncOps[R, A](e: Eff[R, A]): AsyncOps[R, A] = new AsyncOps[R, A](e)

  /**
   * Memoize async values using a cache.
   *
   * A random key is generated for the computation e
   */
  def asyncMemo[R, A](cache: Cache)(e: Eff[R, A])(implicit async: Async /= R): Eff[R, A] =
    asyncMemo(e, cache, UUID.randomUUID.toString)

  /**
   * Memoize async values using a cache
   *
   * if this method is called with the same key the previous value will be returned
   */
  def asyncMemo[R, A](e: Eff[R, A], cache: Cache, key: AnyRef)(implicit async: Async /= R): Eff[R, A] =
    memoizeEffect(e, cache, key)

  /**
   * Memoize async values using a memoization effect
   *
   * if this computation is called twice the results from previous async calls will
   * be retrieved from the cache using the provided key
   */
  def asyncMemoized[R, A](key: AnyRef, e: Eff[R, A])(implicit async: Async /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R].flatMap(cache => asyncMemo(e, cache, key))

  /**
   * Memoize async values using a memoization effect
   *
   * if this computation is called twice the results from previous async calls will
   * be retrieved from the cache using a randomly generated key
   */
  def asyncMemoized[R, A](e: Eff[R, A])(implicit async: Async /= R, m: Memoized |= R): Eff[R, A] = {
    val key = UUID.randomUUID.toString
    MemoEffect.getCache[R].flatMap(cache => asyncMemo(e, cache, key))
  }
}

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
final class AsyncOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def asyncAttempt(implicit async: Async /= R): Eff[R, Throwable Either A] =
    AsyncInterpretation.asyncAttempt(e)

  def asyncMemo(cache: Cache)(implicit async: Async /= R): Eff[R, A] =
    AsyncInterpretation.asyncMemo(cache)(e)
}

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
object AsyncInterpretation extends AsyncInterpretation

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
sealed trait Async[A] extends Any {
  def memoize(key: AnyRef, sequenceKey: Int, cache: Cache): Async[A]
}

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
case class AsyncNow[A](a: A) extends Async[A] {
  def memoize(key: AnyRef, sequenceKey: Int, cache: Cache) = this
}
@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
case class AsyncFailed[A](t: Throwable) extends Async[A] {
  def memoize(key: AnyRef, sequenceKey: Int, cache: Cache) = this
}
@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
case class AsyncDelayed[A](a: Eval[A]) extends Async[A] {
  def memoize(key: AnyRef, sequenceKey: Int, cache: Cache) = AsyncDelayed(a.memoize)
}
@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
case class AsyncEff[A](e: Eff[FS, A], timeout: Option[FiniteDuration] = None) extends Async[A] {
  def memoize(key: AnyRef, sequenceKey: Int, cache: Cache) = AsyncEff(SubscribeEffect.memoize(key, sequenceKey, cache, e), timeout)
}

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
object Async {

  implicit def AsyncCached: SequenceCached[Async] = new SequenceCached[Async] {
    def apply[X](cache: Cache, key: AnyRef, sequenceKey: Int, ax: =>Async[X]): Async[X] =
      ax.memoize(key, sequenceKey, cache)
  }

  def ApplicativeAsync: Applicative[Async] = new Applicative[Async] {

    def pure[A](a: A) = AsyncNow(a)

    def ap[A, B](ff: Async[A => B])(fa: Async[A]): Async[B] =
      (ff, fa) match {
        case (AsyncNow(f), AsyncNow(a)) =>
          AsyncNow(f(a))

        case (AsyncNow(f), AsyncDelayed(a)) =>
          AsyncDelayed(a.map(f))

        case (AsyncNow(f), AsyncEff(a, to)) =>
          AsyncEff(a.map(f), to)

        case (AsyncDelayed(f), AsyncNow(a)) =>
          AsyncDelayed(Eval.later(f.value(a)))

        case (AsyncDelayed(f), AsyncDelayed(a)) =>
          AsyncDelayed(Apply[Eval].ap(f)(a))

        case (AsyncDelayed(f), AsyncEff(a, toa)) =>
          AsyncEff(a.map(f.value), toa)

        case (AsyncEff(f, to), AsyncNow(a)) =>
          AsyncEff(f.map(_(a)), to)

        case (AsyncEff(f, tof), AsyncDelayed(a)) =>
          AsyncEff(f.map(_(a.value)), tof)

        case (_, AsyncFailed(t)) =>
          AsyncFailed(t)

        case (AsyncFailed(t), _) =>
          AsyncFailed(t)

        case (AsyncEff(f, tof), AsyncEff(a, toa)) =>
          AsyncEff(EffApplicative[FS].ap(f)(a), (tof |@| toa).map(_ min _))
      }

    override def toString = "Applicative[Async]"
  }


  implicit final def MonadAsync: Monad[Async] = new Monad[Async] {
    def pure[A](a: A) = AsyncEff(Eff.pure[FS, A](a))

    def flatMap[A, B](aa: Async[A])(f: A => Async[B]): Async[B] =
      aa match {
        case AsyncNow(a) =>
          f(a)

        case AsyncFailed(t) =>
          AsyncFailed(t)

        case AsyncDelayed(a) =>
          Either.catchNonFatal(f(a.value)) match {
            case Left(t)   => AsyncFailed(t)
            case Right(ab) => ab
          }

        case AsyncEff(ea, toa) =>
          AsyncEff[B](ea.flatMap(a => subscribeAsync(f(a))), toa)
      }

    def tailRecM[A, B](a: A)(f: A => Async[Either[A, B]]): Async[B] =
      f(a) match {
        case AsyncNow(Left(a1)) => tailRecM(a1)(f)
        case AsyncNow(Right(b)) => AsyncNow[B](b)
        case AsyncFailed(t)      => AsyncFailed[B](t)
        case AsyncDelayed(ab) =>
          Either.catchNonFatal(ab.value) match {
            case Left(t) => AsyncFailed[B](t)
            case Right(Right(b)) => AsyncNow[B](b)
            case Right(Left(a1)) => tailRecM(a1)(f)
          }
        case AsyncEff(e, to) =>
          e match {
            case Pure(ab, last) => ab match {
              case Left(a1) => tailRecM(a1)(f)
              case Right(b) => AsyncNow[B](b)
            }

            case imp @ (Impure(_, _, _) | ImpureAp(_, _, _)) =>
              AsyncEff(imp.flatMap {
                case Left(a1) => subscribeAsync(tailRecM(a1)(f))
                case Right(b) => Eff.pure(b)
              }, to)
          }
      }

    def subscribeAsync[A](a: Async[A]): Eff[FS, A] =
      a  match {
        case AsyncNow(a1)     => Eff.pure[FS, A](a1)
        case AsyncFailed(t)   => send[Subscribe, FS, A](SimpleSubscribe(c => c(Left(t))))
        case AsyncDelayed(a1) => send[Subscribe, FS, A](SimpleSubscribe(c => c(Either.catchNonFatal(a1.value))))
        case AsyncEff(a1, to) => a1
      }

    override def toString = "Monad[AsyncFuture]"
  }
}

