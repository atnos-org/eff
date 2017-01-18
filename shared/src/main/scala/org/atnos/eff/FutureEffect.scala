package org.atnos.eff

import java.util.concurrent.ScheduledExecutorService

import cats._
import cats.implicits._
import org.atnos.eff.all._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise, TimeoutException}
import scala.util.{Failure, Success}

object FutureCreation extends FutureCreation

  final case class TimedFuture[A](callback: (ScheduledExecutorService, ExecutionContext) => Future[A], timeout: Option[FiniteDuration] = None) {
    @inline def runNow(sexs: ScheduledExecutorService, exc: ExecutionContext): Future[A] = {
      timeout.fold {
        callback(sexs, exc)
      } { t =>
        val promise = Promise[A]
        val timeout = new Runnable {
          override def run(): Unit = {
            val _ = promise.tryFailure(new TimeoutException)
          }
        }
        sexs.schedule(timeout, t.length, t.unit)
        promise.tryCompleteWith(callback(sexs, exc))
        promise.future
      }
    }
  }

  object TimedFuture {

    final def ApplicativeTimedFuture: Applicative[TimedFuture] = new Applicative[TimedFuture] {
      override def pure[A](x: A) = TimedFuture((_, _) => Future.successful(x))
      override def ap[A, B](ff: TimedFuture[(A) => B])(fa: TimedFuture[A]): TimedFuture[B] = {
        val newCallback = { (sexs: ScheduledExecutorService, ec: ExecutionContext) =>
          val ffRan = ff.runNow(sexs, ec)
          val faRan = fa.runNow(sexs, ec)
          faRan.flatMap(a => ffRan.map(f => f(a))(ec))(ec)
        }
        TimedFuture(newCallback)
      }
      override def toString = "Applicative[TimedFuture]"
    }

    implicit final def MonadTimedFuture: Monad[TimedFuture] = new Monad[TimedFuture] {
      override def pure[A](x: A) = TimedFuture((_, _) => Future.successful(x))
      override def flatMap[A, B](fa: TimedFuture[A])(f: (A) => TimedFuture[B]): TimedFuture[B] =
        TimedFuture[B]((sexs, ec) => fa.runNow(sexs, ec).flatMap(f(_).runNow(sexs, ec))(ec))
      override def tailRecM[A, B](a: A)(f: (A) => TimedFuture[Either[A, B]]): TimedFuture[B] =
        TimedFuture[B]({ (sexs, ec) =>
          def loop(va: A): Future[B] = f(va).runNow(sexs, ec).flatMap {
            case Left(na) => loop(na)
            case Right(nb) => Future.successful(nb)
          }(ec)
          loop(a)
        })
      override def toString = "Monad[TimedFuture]"
    }
  }

trait FutureTypes {
  type _future[R] = TimedFuture |= R
  type _Future[R] = TimedFuture <= R
}

trait FutureCreation extends FutureTypes {

  final def fromFutureWithExecutors[R :_future, A](c: (ScheduledExecutorService, ExecutionContext) => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture(c, timeout))

  final def fromFuture[R :_future, A](c: => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => c, timeout))

  final def futureFail[R :_future, A](t: Throwable): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => Future.failed(t)))

  final def futureFromEither[R :_future, A](e: Throwable Either A): Eff[R, A] =
    e.fold(futureFail[R, A], Eff.pure[R, A])

  final def futureDelay[R :_future, A](a: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, ec) => Future(a)(ec), timeout))

  final def futureFork[R :_future, A](a: => A, ec: ExecutionContext, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => Future(a)(ec), timeout))

  final def futureDefer[R :_future, A](a: => Future[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    send[TimedFuture, R, A](TimedFuture((_, _) => a, timeout))

}

trait FutureInterpretation extends FutureTypes {

  def runAsync[A](e: Eff[Fx.fx1[TimedFuture], A])(implicit sexs: ScheduledExecutorService, exc: ExecutionContext): Future[A] =
    Eff.detachA(e)(TimedFuture.MonadTimedFuture, TimedFuture.ApplicativeTimedFuture).runNow(sexs, exc)

  def runSequential[A](e: Eff[Fx.fx1[TimedFuture], A])(implicit sexs: ScheduledExecutorService, exc: ExecutionContext): Future[A] =
    Eff.detach(e).runNow(sexs, exc)

  final def futureAttempt[R, A](e: Eff[R, A])(implicit future: TimedFuture /= R): Eff[R, Throwable Either A] = {
    e match {
      case Pure(a, last) =>
        pure[R, Throwable Either A](Either.right(a)).addLast(last)

      case Impure(u, c, last) =>
        future.extract(u) match {
          case Some(tx) =>
            val union = future.inject(attempt(tx))

            Impure(union, Arrs.singleton { ex: (Throwable Either u.X) =>
              ex match {
                case Right(x) => futureAttempt(c(x))
                case Left(t) => pure(Either.left(t))
              }
            }, last)

          case None => Impure(u, Arrs.singleton((x: u.X) => futureAttempt(c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          future.extract(u) match {
            case Some(tx) => future.inject(attempt(tx).asInstanceOf[TimedFuture[Any]])
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val collected = unions.extract(future)
        val continuation1 = Arrs.singleton[R, List[Any], Throwable Either A] { ls: List[Any] =>
          val xors =
            ls.zipWithIndex.collect { case (a, i) =>
              if (collected.indices.contains(i)) a.asInstanceOf[Throwable Either Any]
              else Either.right(a)
            }.sequence

          xors match {
            case Left(t) => pure(Either.left(t))
            case Right(anys) => futureAttempt(continuation(anys))
          }
        }

        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  final def attempt[A](a: TimedFuture[A]): TimedFuture[Throwable Either A] = {
    TimedFuture[Throwable Either A](callback = (sexs, ec) => {
      val prom = Promise[Throwable Either A]()
      a.runNow(sexs, ec).onComplete { t =>
        prom.success(t match {
          case Failure(ex) => Either.left(ex)
          case Success(v) => Either.right(v)
        })
      }(ec)
      prom.future
    })
  }

  final def memoize[A](key: AnyRef, cache: Cache, future: TimedFuture[A]): TimedFuture[A] =
    TimedFuture { (sexs, ec) =>
      val prom = Promise[A]()
      cache.get[A](key).fold {
        prom.completeWith(future.runNow(sexs, ec).map { v => val _ = cache.put(key, v); v }(ec))
      } { v => prom.success(v) }
      prom.future
    }

  implicit final def toFutureOps[R, A](e: Eff[R, A]): FutureOps[R, A] = new FutureOps[R, A](e)

  /**
    * Memoize future values using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  final def futureMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit future: TimedFuture /= R): Eff[R, A] = {
    e match {
      case Pure(a, last) =>
        Pure(a, last)

      case Impure(u, c, last) =>
        future.extract(u) match {
          case Some(tx) => Impure(future.inject(memoize(key, cache, tx)), Arrs.singleton((x: u.X) => futureMemo(key, cache, c(x))), last)
          case None => Impure(u, Arrs.singleton((x: u.X) => futureMemo(key, cache, c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          future.extract(u) match {
            case Some(tx) => future.inject(memoize(key, cache, tx))
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val continuation1 = Arrs.singleton[R, List[Any], A]((ls: List[Any]) => futureMemo(key, cache, continuation(ls)))
        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  /**
    * Memoize Future values using a memoization effect
    *
    * if this method is called with the same key the previous value will be returned
    */
  final def futureMemoized[R, A](key: AnyRef, e: Eff[R, A])(implicit future: TimedFuture /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R].flatMap(cache => futureMemo(key, cache, e))

}

final class FutureOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def futureAttempt(implicit future: TimedFuture /= R): Eff[R, Throwable Either A] =
    FutureInterpretation.futureAttempt(e)

  def futureMemo(key: AnyRef, cache: Cache)(implicit future: TimedFuture /= R): Eff[R, A] =
    FutureInterpretation.futureMemo(key, cache, e)

  def runAsync(implicit sexs: ScheduledExecutorService, exc: ExecutionContext, ev: Eff[R, A] =:= Eff[Fx.fx1[TimedFuture], A]): Future[A] =
    FutureInterpretation.runAsync(e)

  def runSequential(implicit sexs: ScheduledExecutorService, exc: ExecutionContext, ev: Eff[R, A] =:= Eff[Fx.fx1[TimedFuture], A]): Future[A] =
    FutureInterpretation.runSequential(e)
}

object FutureInterpretation extends FutureInterpretation

trait FutureEffect extends FutureCreation with FutureInterpretation

object FutureEffect extends FutureEffect
