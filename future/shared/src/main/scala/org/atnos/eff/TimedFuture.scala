package org.atnos.eff

import cats.*
import java.util.concurrent.TimeoutException
import org.atnos.eff.concurrent.Scheduler
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.concurrent.duration.FiniteDuration

final case class TimedFuture[A](callback: (Scheduler, ExecutionContext) => Future[A], timeout: Option[FiniteDuration] = None) {
  @inline def runNow(scheduler: Scheduler, ec: ExecutionContext): Future[A] =
    timeout.fold(callback(scheduler, ec)) { t =>
      val promise = Promise[A]()
      val cancelTimeout = scheduler.schedule({ promise.tryFailure(new TimeoutException); () }, t)
      promise.completeWith(callback(scheduler, ec).map(a => { cancelTimeout(); a })(using ec))
      promise.future
    }
}

object TimedFuture {

  final val ApplicativeTimedFuture: Applicative[TimedFuture] = new Applicative[TimedFuture] {
    def pure[A](x: A): TimedFuture[A] =
      TimedFuture((_, _) => Future.successful(x))

    def ap[A, B](ff: TimedFuture[A => B])(fa: TimedFuture[A]): TimedFuture[B] = {
      val newCallback = { (scheduler: Scheduler, ec: ExecutionContext) =>
        val ffRan = ff.runNow(scheduler, ec)
        val faRan = fa.runNow(scheduler, ec)
        faRan.flatMap(a => ffRan.map(f => f(a))(using ec))(using ec)
      }
      TimedFuture(newCallback)
    }

    override def toString = "Applicative[TimedFuture]"
  }

  implicit final val MonadTimedFuture: MonadError[TimedFuture, Throwable] = new MonadError[TimedFuture, Throwable] {
    def pure[A](x: A): TimedFuture[A] =
      TimedFuture((_, _) => Future.successful(x))

    def flatMap[A, B](fa: TimedFuture[A])(f: A => TimedFuture[B]): TimedFuture[B] =
      TimedFuture[B]((scheduler, ec) => fa.runNow(scheduler, ec).flatMap(f(_).runNow(scheduler, ec))(using ec))

    def tailRecM[A, B](a: A)(f: A => TimedFuture[Either[A, B]]): TimedFuture[B] =
      TimedFuture[B] { (scheduler, ec) =>
        def loop(va: A): Future[B] = f(va)
          .runNow(scheduler, ec)
          .flatMap {
            case Left(na) => loop(na)
            case Right(nb) => Future.successful(nb)
          }(using ec)
        loop(a)
      }

    def raiseError[A](e: Throwable): TimedFuture[A] =
      TimedFuture((_, _) => Future.failed(e))

    def handleErrorWith[A](fa: TimedFuture[A])(f: Throwable => TimedFuture[A]): TimedFuture[A] =
      TimedFuture((s, ec) => fa.runNow(s, ec).recoverWith[A] { case t => f(t).runNow(s, ec) }(using ec))

    override def toString = "MonadError[TimedFuture, Throwable]"
  }
}
