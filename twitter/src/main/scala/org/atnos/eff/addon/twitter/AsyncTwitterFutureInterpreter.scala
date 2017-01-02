package org.atnos.eff.addon.twitter

import java.util.concurrent.{TimeoutException, _}

import scala.concurrent.duration.FiniteDuration
import scala.util.{Either, Left, Right}

import cats._
import cats.implicits._
import com.twitter.util.{Future, _}
import io.catbird.util._
import org.atnos.eff.Async._
import org.atnos.eff.SubscribeEffect.{AttemptedSubscribe, _}
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.atnos.eff.{NoFx, interpret, _}

case class AsyncTwitterFutureInterpreter(
  fp: FuturePool = FuturePool.unboundedPool,
  timer: Timer = new ScheduledThreadPoolTimer()
) extends AsyncInterpreter[Future] { outer =>

  def runAsync[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    run(e.detachA(ApplicativeAsync))

  def runSequential[A](e: Eff[Fx.fx1[Async], A]): Future[A] =
    run(e.detach)

  def suspend[R :_async, A](future: =>Future[Eff[R, A]]): Eff[R, A] =
    fromFuture(future).flatten

  def fromFuture[R :_async, A](future: =>Future[A]): Eff[R, A] =
    subscribe[R, A](SimpleSubscribe(callback => {future.respond {
      case Return(a) => callback(Right(a))
      case Throw(t) => callback(Left(t))
    }; ()}), None)

  def run[A](r: Async[A]): Future[A] =
    r match {
      case AsyncNow(a)     => Future(a)
      case AsyncFailed(t)  => Future.exception(t)
      case AsyncDelayed(a) => Either.catchNonFatal(a.value).fold(Future.exception, x => Future(x))
      case AsyncEff(e, to) => subscribeToFuture(e, to).detachA(twitterFutureInstance)
    }

  def subscribeToFutureNat(timeout: Option[FiniteDuration]) = new (Subscribe ~> Future) {
    def startFuture[X](subscribe: Subscribe[X]): (() => Future[X], Callback[X]) = {
      val promise: Promise[X] = Promise[X]()

      val callback = (ta: Throwable Either X) =>
        ta match {
          case Left(t)  => promise.setException(t); ()
          case Right(a) => promise.setValue(a); ()
        }

      (() => { fp(subscribe(callback)); promise }, callback)
    }

    def startTimeout(to: FiniteDuration, onTimeout: =>Unit): Unit =
      if (!to.isFinite || to.length >= 1) {
        timer.doLater(Duration(to.length, to.unit))(onTimeout)
        ()
      }

    def apply[X](subscribe: Subscribe[X]): Future[X] = {
      subscribe.memoizeKey match {
        case Some((k, cache)) =>
          cache.memo(k, apply(subscribe.unmemoize))

        case None =>
          timeout match {
            case None => startFuture(subscribe)._1()

            case Some(to) =>
              subscribe match {
                case SimpleSubscribe(_, _) =>
                  val (future, callback) = startFuture(subscribe)
                  startTimeout(to, { callback(Left(new TimeoutException)); () })
                  future()

                case AttemptedSubscribe(_, _) =>
                  val (future, callback) = startFuture(subscribe)
                  startTimeout(to, { callback(Right(Left(new TimeoutException))); () })
                  future()
              }

          }
      }

    }
  }

  def subscribeToFuture[A](e: Eff[Fx1[Subscribe], A], timeout: Option[FiniteDuration])(implicit m: Subscribe <= Fx1[Subscribe]): Eff[Fx1[Future], A] =
    interpret.transform[Fx1[Subscribe], Fx1[Future], NoFx, Subscribe, Future, A](e, subscribeToFutureNat(timeout))

  implicit final def toRunAsyncFutureOps[A](e: Eff[Fx.fx1[Async], A]): RunAsyncFutureOps[A] =
    new RunAsyncFutureOps[A](e)

  final class RunAsyncFutureOps[A](val e: Eff[Fx.fx1[Async], A]) {

    def runAsyncFuture: Future[A] =
      outer.runAsync(e)

    def runAsyncSequential: Future[A] =
      outer.runSequential(e)
  }
}
