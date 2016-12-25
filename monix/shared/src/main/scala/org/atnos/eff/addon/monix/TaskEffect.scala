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
        override def onSuccess(value: A): Unit = {
          val _ = promise.trySuccess(value)
        }
        override def onError(ex: Throwable): Unit = {
          val _ = promise.tryFailure(ex)
        }
      })
      promise.future.onComplete(callback)(context.scheduler)
    }
  }
}

object TimedTask {
  final def TimedTaskApplicative: Applicative[TimedTask] = new Applicative[TimedTask] {
    override def pure[A](x: A) = TimedTask(_ => Task.now(x))
    override def ap[A, B](ff: TimedTask[(A) => B])(fa: TimedTask[A]) =
      TimedTask(sexs => Task.mapBoth(ff.runNow(sexs), fa.runNow(sexs))(_ (_)))
  }

  implicit final def TimedTaskMonad: Monad[TimedTask] = new Monad[TimedTask] {
    override def pure[A](x: A) = TimedTask(_ => Task.now(x))
    override def flatMap[A, B](fa: TimedTask[A])(f: (A) => TimedTask[B]) =
      TimedTask(sexs => fa.runNow(sexs).flatMap(f(_).runNow(sexs)))
    override def tailRecM[A, B](a: A)(f: (A) => TimedTask[Either[A, B]]): TimedTask[B] =
      TimedTask[B]({ sexs =>
        def loop(na: A): Task[B] = f(na).runNow(sexs).flatMap(_.fold(loop, Task.now))
        loop(a)
      })
  }

}

trait TaskTypes {
  type _task[R] = |=[TimedTask, R]
  type _Task[R] = <=[TimedTask, R]
}

trait TaskCreation extends TaskTypes {

  final def taskWithExecutors[R: _task, A](c: ScheduledExecutorService => Task[A],
                                           timeout: Option[FiniteDuration] = None): Eff[R, A] =
    Eff.send[TimedTask, R, A](TimedTask(c, timeout))

  final def taskFailed[R: _task, A](t: Throwable): Eff[R, A] =
    TimedTask(_ => Task.fromTry[A](Failure(t))).send

  final def taskSuspend[R: _task, A](task: => Task[Eff[R, A]], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.suspend(task), timeout).send.flatten

  final def taskDelay[R: _task, A](call: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.delay(call), timeout).send

  final def taskForkScheduler[R: _task, A](call: Task[A], scheduler: Scheduler, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.fork(call, scheduler), timeout).send

  final def taskFork[R: _task, A](call: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.fork(call), timeout).send

  final def async[R: _task, A](callbackConsumer: ((Throwable Either A) => Unit) => Unit,
                               timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.async[A] { (_, cb) =>
      callbackConsumer(tea => cb(tea.fold(Failure(_), Success(_)))); Cancelable.empty
    }, timeout).send

  final def asyncCancelable[R: _task, A](callbackConsumer: ((Throwable Either A) => Unit) => Cancelable,
                                         timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.async[A] { (_, cb) =>
      callbackConsumer(tea => cb(tea.fold(Failure(_), Success(_))))
    }, timeout).send

}

object TaskCreation extends TaskCreation

trait TaskInterpretation extends TaskTypes {

  def attempt[A](task: TimedTask[A]): TimedTask[Throwable Either A] = {
    TimedTask[Throwable Either A](sexs => task.runNow(sexs).materialize.map(t => Either.cond(t.isSuccess, t.get, t.failed.get)))
  }

  def taskAttempt[R, A](e: Eff[R, A])(implicit task: TimedTask /= R): Eff[R, Throwable Either A] = {
    e match {
      case Pure(a, last) =>
        Eff.pure[R, Throwable Either A](Either.right(a)).addLast(last)

      case Impure(u, c, last) =>
        task.extract(u) match {
          case Some(tx) =>
            val union = task.inject(attempt(tx))

            Impure(union, Arrs.singleton { ex: (Throwable Either u.X) =>
              ex match {
                case Right(x) => taskAttempt(c(x))
                case Left(t) => Eff.pure(Either.left(t))
              }
            }, last)

          case None => Impure(u, Arrs.singleton((x: u.X) => taskAttempt(c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          task.extract(u) match {
            case Some(tx) => task.inject(attempt(tx).asInstanceOf[TimedTask[Any]])
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val collected = unions.extract(task)
        val continuation1 = Arrs.singleton[R, List[Any], Throwable Either A] { ls: List[Any] =>
          val xors =
            ls.zipWithIndex.collect { case (a, i) =>
              if (collected.indices.contains(i)) a.asInstanceOf[Throwable Either Any]
              else Either.right(a)
            }.sequence

          xors match {
            case Left(t) => Eff.pure(Either.left(t))
            case Right(anys) => taskAttempt(continuation(anys))
          }
        }

        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  def memoize[A](key: AnyRef, cache: Cache, task: Task[A]): Task[A] =
    Task.suspend {
      cache.get[A](key).fold(task.map { r => cache.put(key, r); r })(Task.now)
    }

  implicit final def toTaskOps[R, A](e: Eff[R, A]): TaskOps[R, A] = new TaskOps[R, A](e)

  /**
    * Memoize tasks using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  def taskMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit task: TimedTask /= R): Eff[R, A] = {
    e match {
      case Pure(a, last) =>
        Pure(a, last)

      case Impure(u, c, last) =>
        task.extract(u) match {
          case Some(tx) => Impure(task.inject(tx.copy(task = tx.task.andThen(memoize(key, cache, _)))), Arrs.singleton((x: u.X) => taskMemo(key, cache, c(x))), last)
          case None => Impure(u, Arrs.singleton((x: u.X) => taskMemo(key, cache, c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          task.extract(u) match {
            case Some(tx) => task.inject(tx.copy(task = tx.task.andThen(memoize(key, cache, _))))
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val continuation1 = Arrs.singleton[R, List[Any], A]((ls: List[Any]) => taskMemo(key, cache, continuation(ls)))
        ImpureAp(materializedUnions, continuation1, last)
    }
  }

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

final class TaskOps[R, A](val e: Eff[R, A]) extends AnyVal {

  def runTaskMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], task: TimedTask |= U): Eff[U, A] = {
    TaskEffect.runTaskMemo(cache)(effect)

  def taskAttempt(implicit task: TimedTask /= R): Eff[R, Throwable Either A] =
    TaskInterpretation.taskAttempt(e)

  def taskMemo(key: AnyRef, cache: Cache)(implicit task: TimedTask /= R): Eff[R, A] =
    TaskInterpretation.taskMemo(key, cache, e)
}

object TaskInterpretation extends TaskInterpretation

trait TaskEffect extends TaskInterpretation with TaskCreation

object TaskEffect extends TaskEffect
