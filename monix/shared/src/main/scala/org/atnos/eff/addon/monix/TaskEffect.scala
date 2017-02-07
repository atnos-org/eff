package org.atnos.eff.addon.monix

import java.util.concurrent.ScheduledExecutorService

import cats._
import cats.implicits._
import monix.eval._
import monix.execution._
import org.atnos.eff._
import org.atnos.eff.syntax.all._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Promise, TimeoutException}
import scala.util._

case class TimedTask[A](task: ScheduledExecutorService => Task[A], timeout: Option[FiniteDuration] = None) {
  def runNow(sexs: ScheduledExecutorService): Task[A] = timeout.fold(task(sexs)) { t =>
    Task.unsafeCreate[A] { (context, callback) =>
      val promise = Promise[A]
      val onTimeout = new Runnable {
        override def run(): Unit = {
          val _ = promise.tryFailure(new TimeoutException)
        }
      }
      sexs.schedule(onTimeout, t.length, t.unit)
      Task.unsafeStartAsync(task(sexs), context, new Callback[A] {
        def onSuccess(value: A): Unit = {
          val _ = promise.trySuccess(value)
        }

        def onError(ex: Throwable): Unit = {
          val _ = promise.tryFailure(ex)
        }
      })
      promise.future.onComplete(callback)(context.scheduler)
    }
  }
}

object TimedTask {
  final def TimedTaskApplicative: Applicative[TimedTask] = new Applicative[TimedTask] {
    def pure[A](x: A) = TimedTask(_ => Task.now(x))

    def ap[A, B](ff: TimedTask[(A) => B])(fa: TimedTask[A]) =
      TimedTask(sexs => Task.mapBoth(ff.runNow(sexs), fa.runNow(sexs))(_ (_)))
  }

  implicit final def TimedTaskMonad: Monad[TimedTask] = new Monad[TimedTask] {
    def pure[A](x: A) = TimedTask(_ => Task.now(x))

    def flatMap[A, B](fa: TimedTask[A])(f: (A) => TimedTask[B]) =
      TimedTask(sexs => fa.runNow(sexs).flatMap(f(_).runNow(sexs)))

    def tailRecM[A, B](a: A)(f: (A) => TimedTask[Either[A, B]]): TimedTask[B] =
      TimedTask[B]({ sexs =>
        def loop(na: A): Task[B] = f(na).runNow(sexs).flatMap(_.fold(loop, Task.now))
        loop(a)
      })
  }

  final def now[A](value: A): TimedTask[A] =
    TimedTask(_ => Task.now(value))
  implicit final def fromTask[A](task: Task[A]): TimedTask[A] =
    TimedTask(_ => task)
  final def fromTask[A](task: Task[A], timeout: Option[FiniteDuration] = None): TimedTask[A] =
    TimedTask(_ => task, timeout)

}

trait TaskTypes {
  type _task[R] = |=[TimedTask, R]
  type _Task[R] = <=[TimedTask, R]
}

trait TaskCreation extends TaskTypes {

  final def taskWithContext[R :_task, A](c: ScheduledExecutorService => Task[A],
                                           timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(c, timeout).send

  final def fromTask[R :_task, A](task: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => task, timeout).send[R]

  final def taskFailed[R :_task, A](t: Throwable): Eff[R, A] =
    TimedTask(_ => Task.fromTry[A](Failure(t))).send

  final def taskSuspend[R :_task, A](task: => Task[Eff[R, A]], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.suspend(task), timeout).send.flatten

  final def taskDelay[R :_task, A](call: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.delay(call), timeout).send

  final def taskForkScheduler[R :_task, A](call: Task[A], scheduler: Scheduler, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.fork(call, scheduler), timeout).send

  final def taskFork[R :_task, A](call: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.fork(call), timeout).send

  final def taskAsync[R :_task, A](callbackConsumer: ((Throwable Either A) => Unit) => Unit,
                               timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.async[A] { (_, cb) =>
      callbackConsumer(tea => cb(tea.fold(Failure(_), Success(_)))); Cancelable.empty
    }, timeout).send

  final def taskAsyncScheduler[R :_task, A](callbackConsumer: ((Throwable Either A) => Unit) => Cancelable,
                                            scheduler: Scheduler,
                                             timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.async[A] { (_, cb) =>
      callbackConsumer(tea => cb(tea.fold(Failure(_), Success(_))))
    }, timeout).send

}

object TaskCreation extends TaskCreation

trait TaskInterpretation extends TaskTypes {

  def runAsync[A](e: Eff[Fx.fx1[TimedTask], A])(implicit sexs: ScheduledExecutorService): Task[A] =
    Eff.detachA(e)(TimedTask.TimedTaskMonad, TimedTask.TimedTaskApplicative).runNow(sexs)

  def runSequential[A](e: Eff[Fx.fx1[TimedTask], A])(implicit sexs: ScheduledExecutorService): Task[A] =
    Eff.detach(e).runNow(sexs)

  def attempt[A](task: TimedTask[A]): TimedTask[Throwable Either A] = {
    TimedTask[Throwable Either A](sexs => task.runNow(sexs).materialize.map(t => Either.cond(t.isSuccess, t.get, t.failed.get)))
  }

  import interpret.of

  def taskAttempt[R, A](e: Eff[R, A])(implicit task: TimedTask /= R): Eff[R, Throwable Either A] =
    interpret.interceptNatM[R, TimedTask, Throwable Either ?, A](e,
      new (TimedTask ~> (TimedTask of (Throwable Either ?))#l) {
        override def apply[X](fa: TimedTask[X]): TimedTask[Throwable Either X] =
          attempt(fa)
      })

  def memoize[A](key: AnyRef, cache: Cache, task: Task[A]): Task[A] =
    Task.suspend {
      cache.get[A](key).fold(task.map { r => cache.put(key, r); r })(Task.now)
    }

  /**
    * Memoize tasks using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  def taskMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit task: TimedTask /= R): Eff[R, A] =
    interpret.interceptNat[R, TimedTask, A](e)(
      new (TimedTask ~> TimedTask) {
        override def apply[X](fa: TimedTask[X]): TimedTask[X] =
          fa.copy(task = fa.task.andThen(memoize(key, cache, _)))
      }
    )

  /**
    * Memoize task values using a memoization effect
    *
    * if this method is called with the same key the previous value will be returned
    */
  def taskMemoized[R, A](key: AnyRef, e: Eff[R, A])(implicit task: TimedTask /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R].flatMap(cache => taskMemo(key, cache, e))

  def runTaskMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], task: TimedTask |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => TaskCreation.taskDelay(cache.memo(key, value()))
          case GetCache()        => TaskCreation.taskDelay(cache)
        }
    })
  }

}

object TaskInterpretation extends TaskInterpretation

trait TaskEffect extends TaskInterpretation with TaskCreation

object TaskEffect extends TaskEffect
