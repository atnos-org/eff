package org.atnos.eff

import cats._
import cats.implicits._
import org.atnos.eff.all._

import scala.concurrent.duration.FiniteDuration
import SubscribeEffect._

trait AsyncEffect extends AsyncCreation

object AsyncEffect extends AsyncEffect

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

object AsyncCreation extends AsyncCreation

trait AsyncInterpretation {

  def asyncAttempt[R, A](e: Eff[R, A])(implicit async: Async /= R): Eff[R, Throwable Either A] = {
    e match {
      case Pure(a, last) =>
        pure[R, Throwable Either A](Either.right(a)).addLast(last)

      case Impure(u, c, last) =>
        async.extract(u) match {
          case Some(tx) =>
            val union = async.inject(attempt(tx))

            Impure(union, Arrs.singleton { ex: (Throwable Either u.X) =>
              ex match {
                case Right(x) => asyncAttempt(c(x))
                case Left(t) => pure(Either.left(t))
              }
            }, last)

          case None => Impure(u, Arrs.singleton((x: u.X) => asyncAttempt(c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          async.extract(u) match {
            case Some(tx) => async.inject(attempt(tx).asInstanceOf[Async[Any]])
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val collected = unions.extract(async)
        val continuation1 = Arrs.singleton[R, List[Any], Throwable Either A] { ls: List[Any] =>
          val xors =
            ls.zipWithIndex.collect { case (a, i) =>
              if (collected.indices.contains(i)) a.asInstanceOf[Throwable Either Any]
              else Either.right(a)
            }.sequence

          xors match {
            case Left(t)     => pure(Either.left(t))
            case Right(anys) => asyncAttempt(continuation(anys))
          }
        }

        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  def attempt[A](a: Async[A]): Async[Throwable Either A] =
    a match {
      case AsyncNow(a1)     => AsyncNow[Throwable Either A](Right(a1))
      case AsyncFailed(t)   => AsyncNow[Throwable Either A](Left(t))
      case AsyncDelayed(a1) => AsyncDelayed(Eval.later(Either.catchNonFatal(a1.value)))
      case AsyncEff(e, to)  => AsyncEff(subscribeAttempt(e), to)
    }

  implicit final def toAsyncOps[R, A](e: Eff[R, A]): AsyncOps[R, A] = new AsyncOps[R, A](e)

  /**
   * Memoize async values using a cache
   *
   * if this method is called with the same key the previous value will be returned
   */
  def asyncMemo[R, K <: AnyRef, A](key: K, cache: Cache[K], e: Eff[R, A])(implicit async: Async /= R): Eff[R, A] = {
    e match {
      case Pure(a, last) =>
        Pure(a, last)

      case Impure(u, c, last) =>
        async.extract(u) match {
          case Some(tx) => Impure(async.inject(memo(key, cache, tx)), Arrs.singleton((x: u.X) => asyncMemo(key, cache, c(x))), last)
          case None     => Impure(u, Arrs.singleton((x: u.X) => asyncMemo(key, cache, c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          async.extract(u) match {
            case Some(tx) => async.inject(memo(key, cache, tx))
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val continuation1 = Arrs.singleton[R, List[Any], A]((ls: List[Any]) => asyncMemo(key, cache, continuation(ls)))
        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  /**
   * Memoize async values using a memoization effect
   *
   * if this method is called with the same key the previous value will be returned
   */
  def asyncMemoized[R, K <: AnyRef, A](key: K, e: Eff[R, A])(implicit async: Async /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R, K].flatMap(cache => asyncMemo(key, cache, e))

  private def memo[K <: AnyRef, A](k: K, cache: Cache[K], a: Async[A]): Async[A] =
    a.memoize(k, cache)

}

final class AsyncOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def asyncAttempt(implicit async: Async /= R): Eff[R, Throwable Either A] =
    AsyncInterpretation.asyncAttempt(e)

  def asyncMemo[K <: AnyRef](key: K, cache: Cache[K])(implicit async: Async /= R): Eff[R, A] =
    AsyncInterpretation.asyncMemo(key, cache, e)
}

object AsyncInterpretation extends AsyncInterpretation

sealed trait Async[A] extends Any {
  def memoize[K <: AnyRef](key: K, cache: Cache[K]): Async[A]
}

case class AsyncNow[A](a: A) extends Async[A] {
  def memoize[K <: AnyRef](key: K, cache: Cache[K]) = this
}
case class AsyncFailed[A](t: Throwable) extends Async[A] {
  def memoize[K <: AnyRef](key: K, cache: Cache[K]) = this
}
case class AsyncDelayed[A](a: Eval[A]) extends Async[A] {
  def memoize[K <: AnyRef](key: K, cache: Cache[K]) = AsyncDelayed(a.memoize)
}
case class AsyncEff[A](e: Eff[FS, A], timeout: Option[FiniteDuration] = None) extends Async[A] {
  def memoize[K <: AnyRef](key: K, cache: Cache[K]) = AsyncEff(SubscribeEffect.memoize(key, cache, e), timeout)
}

object Async {

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

            case imp @ Impure(u, c, last) =>
              AsyncEff(imp.flatMap {
                case Left(a1) => subscribeAsync(tailRecM(a1)(f))
                case Right(b) => Eff.pure(b)
              }, to)

            case imp @ ImpureAp(unions, continuation, last) =>
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

