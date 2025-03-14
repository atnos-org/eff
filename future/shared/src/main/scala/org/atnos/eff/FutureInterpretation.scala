package org.atnos.eff

import cats.*
import org.atnos.eff.concurrent.Scheduler
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise

trait FutureInterpretation extends FutureTypes {

  def runAsyncOn[R, A](executorServices: ExecutorServices)(e: Eff[R, A])(implicit m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    runAsync(e)(using executorServices.scheduler, executorServices.executionContext, m)

  def runAsync[R, A](e: Eff[R, A])(implicit scheduler: Scheduler, exc: ExecutionContext, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    Eff.detachA(Eff.effInto[R, Fx1[TimedFuture], A](e))(using TimedFuture.MonadTimedFuture, TimedFuture.ApplicativeTimedFuture).runNow(scheduler, exc)

  def runSequentialOn[R, A](executorServices: ExecutorServices)(e: Eff[R, A])(implicit m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    runSequential(e)(using executorServices.scheduler, executorServices.executionContext, m)

  def runSequential[R, A](e: Eff[R, A])(implicit scheduler: Scheduler, exc: ExecutionContext, m: Member.Aux[TimedFuture, R, NoFx]): Future[A] =
    Eff.detach(Eff.effInto[R, Fx1[TimedFuture], A](e)).runNow(scheduler, exc)

  final def futureAttempt[R, A](e: Eff[R, A])(implicit future: TimedFuture /= R): Eff[R, Either[Throwable, A]] =
    interpret.interceptNatM[R, TimedFuture, Either[Throwable, *], A](
      e,
      new (TimedFuture ~> ({ type l[a] = TimedFuture[Either[Throwable, a]] })#l) {
        override def apply[X](fa: TimedFuture[X]): TimedFuture[Either[Throwable, X]] = attempt(fa)
      }
    )

  final def attempt[A](a: TimedFuture[A]): TimedFuture[Either[Throwable, A]] = {
    TimedFuture[Either[Throwable, A]](callback = (scheduler, ec) => {
      val prom = Promise[Either[Throwable, A]]()
      a.runNow(scheduler, ec)
        .onComplete { t =>
          prom.success(t.toEither)
        }(using ec)
      prom.future
    })
  }

  final def memoize[A](key: AnyRef, cache: Cache, future: TimedFuture[A]): TimedFuture[A] =
    TimedFuture { (scheduler, ec) =>
      val prom = Promise[A]()
      cache
        .get[A](key)
        .fold {
          prom.completeWith(
            future
              .runNow(scheduler, ec)
              .map { v =>
                val _ = cache.put(key, v); v
              }(using ec)
          )
        } { v => prom.success(v) }
      prom.future
    }

  /**
    * Memoize future values using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  final def futureMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit future: TimedFuture /= R): Eff[R, A] =
    interpret.interceptNat[R, TimedFuture, A](e)(
      new (TimedFuture ~> TimedFuture) {
        override def apply[X](fa: TimedFuture[X]): TimedFuture[X] = memoize(key, cache, fa)
      }
    )

  /**
    * Memoize Future values using a memoization effect
    *
    * if this method is called with the same key the previous value will be returned
    */
  final def futureMemoized[R, A](key: AnyRef, e: Eff[R, A])(implicit future: TimedFuture /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R].flatMap(cache => futureMemo(key, cache, e))

}

object FutureInterpretation extends FutureInterpretation
