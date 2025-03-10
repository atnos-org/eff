package org.atnos.eff.syntax.addon.twitter

import com.twitter.util.Future
import com.twitter.util.FuturePool
import org.atnos.eff.addon.twitter._
import org.atnos.eff._
import org.atnos.eff.concurrent.Scheduler
import scala.concurrent.duration.FiniteDuration

trait future {

  implicit final def toTwitterFutureOps[R, A](e: Eff[R, A]): TwitterFutureOps[R, A] = new TwitterFutureOps[R, A](e)

}

object future extends future

final class TwitterFutureOps[R, A](private val e: Eff[R, A]) extends AnyVal {

  def runTwitterFutureMemo[U](cache: Cache)(implicit memMember: Member.Aux[Memoized, R, U], futMember: TwitterTimedFuture |= U): Eff[U, A] =
    TwitterFutureEffect.runFutureMemo(cache)(e)(using memMember, futMember)

  def twitterFutureAttempt(implicit future: TwitterTimedFuture /= R): Eff[R, Throwable Either A] =
    TwitterFutureInterpretation.futureAttempt(e)

  def twitterFutureMemo(key: AnyRef, cache: Cache)(implicit future: TwitterTimedFuture /= R): Eff[R, A] =
    TwitterFutureInterpretation.futureMemo(key, cache, e)

  def runAsync(implicit pool: FuturePool, scheduler: Scheduler, m: Member.Aux[TwitterTimedFuture, R, NoFx]): Future[A] =
    TwitterFutureInterpretation.runAsync(e)

  def runSequential(implicit pool: FuturePool, scheduler: Scheduler, m: Member.Aux[TwitterTimedFuture, R, NoFx]): Future[A] =
    TwitterFutureInterpretation.runSequential(e)

  def retryUntil(condition: A => Boolean, durations: List[FiniteDuration])(implicit task: TwitterTimedFuture |= R): Eff[R, A] =
    TwitterFutureCreation.retryUntil(e, condition, durations)
}
