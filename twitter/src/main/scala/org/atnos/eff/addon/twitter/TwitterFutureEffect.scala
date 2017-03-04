package org.atnos.eff.addon.twitter

import java.util.concurrent.TimeUnit
import java.util.{Timer, TimerTask}

import cats._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.{NoFx, interpret, _}
import io.catbird.util._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, TimeoutException}
import com.twitter.util._

object TwitterFutureCreation extends TwitterFutureCreation

final case class TwitterTimedFuture[A](callback: (FuturePool, Timer) => Future[A], timeout: Option[FiniteDuration] = None) {
  @inline def runNow(pool: FuturePool, timer: Timer): Future[A] = {
    timeout.fold {
      callback(pool, sexs)
    } { t =>
      val promise = Promise[A]
      val timeout = new TimerTask {
        override def run(): Unit = {
          val _ = promise.updateIfEmpty(Throw(new TimeoutException))
        }
      }
      sexs.schedule(timeout, TimeUnit.MILLISECONDS.convert(t.length, t.unit))
      callback(pool, sexs)
        .onFailure { e => val _ = promise.updateIfEmpty(Throw(e)) }
        .onSuccess { a => val _ = promise.updateIfEmpty(Return(a)) }
      promise
    }
  }
}

object TwitterTimedFuture {

  final def ApplicativeTwitterTimedFuture: Applicative[TwitterTimedFuture] = new Applicative[TwitterTimedFuture] {
    override def pure[A](x: A) = TwitterTimedFuture((_, _) => Future.value(x))
    override def ap[A, B](ff: TwitterTimedFuture[(A) => B])(fa: TwitterTimedFuture[A]): TwitterTimedFuture[B] = {
      val newCallback = { (pool: FuturePool, timer: Timer) =>
        val ffRan = ff.runNow(pool, sexs)
        val faRan = fa.runNow(pool, sexs)
        ffRan.joinWith(faRan)(_(_))
      }
      TwitterTimedFuture(newCallback)
    }
    override def toString = "Applicative[TwitterTimedFuture]"
  }

  implicit final def MonadTwitterTimedFuture: Monad[TwitterTimedFuture] = new Monad[TwitterTimedFuture] {
    override def pure[A](x: A) = TwitterTimedFuture((_, _) => Future.value(x))
    override def flatMap[A, B](fa: TwitterTimedFuture[A])(f: (A) => TwitterTimedFuture[B]): TwitterTimedFuture[B] =
      TwitterTimedFuture[B]((pool, sexs) => fa.runNow(pool, sexs).flatMap(f(_).runNow(pool, sexs)))
    override def tailRecM[A, B](a: A)(f: (A) => TwitterTimedFuture[Either[A, B]]): TwitterTimedFuture[B] =
      TwitterTimedFuture[B]({ (pool, sexs) =>
        def loop(va: A): Future[B] = f(va).runNow(pool, sexs).flatMap {
          case Left(na) => loop(na)
          case Right(nb) => Future.value(nb)
        }
        loop(a)
      })
    override def toString = "Monad[TwitterTimedFuture]"
  }
}

trait TwitterFutureTypes {
  type _future[R] = TwitterTimedFuture |= R
  type _Future[R] = TwitterTimedFuture <= R
}

trait TwitterFutureCreation extends TwitterFutureTypes {

  final def fromFutureWithExecutors[R :_future, A](c: (FuturePool, Timer) => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TwitterTimedFuture, R, A](TwitterTimedFuture(c, timeout))

  final def fromFuture[R :_future, A](c: => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TwitterTimedFuture, R, A](TwitterTimedFuture((_, _) => c, timeout))

  final def futureFail[R :_future, A](t: Throwable): Eff[R, A] =
    send[TwitterTimedFuture, R, A](TwitterTimedFuture((_, _) => Future.exception(t)))

  final def futureFromEither[R :_future, A](e: Throwable Either A): Eff[R, A] =
    e.fold(futureFail[R, A], Eff.pure[R, A])

  final def futureDelay[R :_future, A](a: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TwitterTimedFuture, R, A](TwitterTimedFuture((pool, _) => pool(a), timeout))

  final def futureFork[R :_future, A](a: => A)(pool: FuturePool, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TwitterTimedFuture, R, A](TwitterTimedFuture((_, _) => pool(a), timeout))

}

trait TwitterFutureInterpretation extends TwitterFutureTypes {

  def runAsync[A](e: Eff[Fx.fx1[TwitterTimedFuture], A])(implicit pool: FuturePool, timer: Timer): Future[A] =
    Eff.detachA(e)(TwitterTimedFuture.MonadTwitterTimedFuture, TwitterTimedFuture.ApplicativeTwitterTimedFuture).runNow(pool, sexs)

  def runSequential[A](e: Eff[Fx.fx1[TwitterTimedFuture], A])(implicit pool: FuturePool, timer: Timer): Future[A] =
    Eff.detach(e).runNow(pool, sexs)

  import interpret.of

  final def futureAttempt[R, A](e: Eff[R, A])(implicit future: TwitterTimedFuture /= R): Eff[R, Throwable Either A] =
    interpret.interceptNatM[R, TwitterTimedFuture, Throwable Either ?, A](e,
      new (TwitterTimedFuture ~> (TwitterTimedFuture of (Throwable Either ?))#l) {
        override def apply[X](fa: TwitterTimedFuture[X]): TwitterTimedFuture[Throwable Either X] = attempt(fa)
      })

  final def attempt[A](a: TwitterTimedFuture[A]): TwitterTimedFuture[Throwable Either A] = {
    TwitterTimedFuture[Throwable Either A](callback =
      a.runNow(_, _).liftToTry.map {
        case Throw(ex) => Either.left(ex)
        case Return(v) => Either.right(v)
      }
    )
  }

  final def memoize[A](key: AnyRef, cache: Cache, future: TwitterTimedFuture[A]): TwitterTimedFuture[A] =
    TwitterTimedFuture { (pool, sexs) =>
      val prom = Promise[A]()
      cache.get[A](key).fold {
        future.runNow(pool, sexs).map { v => val _ = cache.put(key, v); v }.proxyTo(prom)
      } { v => prom.setValue(v) }
      prom
    }

  /**
    * Memoize future values using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  final def futureMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit future: TwitterTimedFuture /= R): Eff[R, A] =
    interpret.interceptNat[R, TwitterTimedFuture, A](e)(
      new (TwitterTimedFuture ~> TwitterTimedFuture) {
        override def apply[X](fa: TwitterTimedFuture[X]): TwitterTimedFuture[X] = memoize(key, cache, fa)
      }
    )

  /**
    * Memoize Future values using a memoization effect
    *
    * if this method is called with the same key the previous value will be returned
    */
  final def futureMemoized[R, A](key: AnyRef, e: Eff[R, A])(implicit future: TwitterTimedFuture /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R].flatMap(cache => futureMemo(key, cache, e))

  def runFutureMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], task: TwitterTimedFuture |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => TwitterFutureEffect.futureDelay(cache.memo(key, value()))
          case GetCache()        => TwitterFutureEffect.futureDelay(cache)
        }
    })
  }

}

object TwitterFutureInterpretation extends TwitterFutureInterpretation

trait TwitterFutureEffect extends TwitterFutureCreation with TwitterFutureInterpretation

object TwitterFutureEffect extends TwitterFutureEffect
