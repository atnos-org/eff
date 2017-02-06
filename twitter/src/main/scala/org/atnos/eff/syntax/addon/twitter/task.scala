package org.atnos.eff.syntax.addon.twitter

import java.util.concurrent.ScheduledExecutorService

import com.twitter.util.{Future, FuturePool}
import org.atnos.eff.addon.twitter._
import org.atnos.eff.{Fx, _}

trait task {

  implicit final def toTwitterFutureOps[R, A](e: Eff[R, A]): TwitterFutureOps[R, A] = new TwitterFutureOps[R, A](e)

}

object task extends task

final class TwitterFutureOps[R, A](val e: Eff[R, A]) extends AnyVal {

  def runTwitterFutureMemo[U](cache: Cache)(implicit memMember: Member.Aux[Memoized, R, U],
                                            futMember: TwitterTimedFuture |= U): Eff[U, A] =
    TwitterFutureEffect.runFutureMemo(cache)(e)(memMember, futMember)

  def twitterFutureAttempt(implicit future: TwitterTimedFuture /= R): Eff[R, Throwable Either A] =
    TwitterFutureInterpretation.futureAttempt(e)

  def twitterFutureMemo(key: AnyRef, cache: Cache)(implicit future: TwitterTimedFuture /= R): Eff[R, A] =
    TwitterFutureInterpretation.futureMemo(key, cache, e)

  def runAsync(implicit pool: FuturePool, sexs: ScheduledExecutorService, ev: Eff[R, A] =:= Eff[Fx.fx1[TwitterTimedFuture], A]): Future[A] =
    TwitterFutureInterpretation.runAsync(e)

  def runSequential(implicit pool: FuturePool, sexs: ScheduledExecutorService, ev: Eff[R, A] =:= Eff[Fx.fx1[TwitterTimedFuture], A]): Future[A] =
    TwitterFutureInterpretation.runSequential(e)
}
