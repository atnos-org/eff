package org.atnos.eff.addon.scalaz.concurrent

import java.util.concurrent.{ExecutorService, ScheduledExecutorService}

import org.atnos.eff.syntax.all._

import scalaz.{-\/, Nondeterminism, \/, \/-}
import scalaz.concurrent._
import cats._
import cats.implicits._
import org.atnos.eff._

import scala.concurrent.{ExecutionContext, Promise, TimeoutException}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Either, Failure, Success, Try}

case class TimedTask[A](task: (ScheduledExecutorService, ExecutionContext) => Task[A], timeout: Option[FiniteDuration] = None) {
  @inline def runNow(sexs: ScheduledExecutorService, ec: ExecutionContext): Task[A] = timeout.fold(task(sexs, ec)) { t =>
    Task.async[A] { register =>
      val promise = Promise[A]
      val onTimeout = new Runnable {
        override def run(): Unit = {
          val _ = promise.tryFailure(new TimeoutException)
        }
      }
      val _ = sexs.schedule(onTimeout, t.length, t.unit)
      task(sexs, ec).unsafePerformAsync { tea =>
        val _ = promise.tryComplete(tea.fold(Failure(_), Success(_)))
      }
      promise.future.onComplete(t => register(if (t.isSuccess) \/-(t.get) else -\/(t.failed.get)))(ec)
    }
  }
}

object TimedTask {

  def TimedTaskApplicative: Applicative[TimedTask] = new Applicative[TimedTask] {
    def pure[A](x: A): TimedTask[A] =
      TimedTask((_, _) => Task.now(x))

    def ap[A, B](ff: TimedTask[A => B])(fa: TimedTask[A]): TimedTask[B] =
      TimedTask[B]((sexs, ec) => Nondeterminism[Task].mapBoth(ff.runNow(sexs, ec), fa.runNow(sexs, ec))(_(_)))

    override def toString = "Applicative[Task]"
  }

  implicit def TimedTaskMonad: Monad[TimedTask] = new Monad[TimedTask] {
    def pure[A](x: A): TimedTask[A] =
      TimedTask((_, _) => Task.now(x))

    def flatMap[A, B](fa: TimedTask[A])(f: A => TimedTask[B]): TimedTask[B] =
      TimedTask((sexs, ec) => fa.runNow(sexs, ec).flatMap(f(_).runNow(sexs, ec)))

    def tailRecM[A, B](a: A)(f: A => TimedTask[Either[A, B]]): TimedTask[B] =
      TimedTask({ (sexs, ec) =>
        def loop(na: A): Task[B] = { f(na).runNow(sexs, ec).flatMap(_.fold(loop, Task.now)) }
        loop(a)
      })

    override def toString = "Monad[Task]"

  }

  final def now[A](value: A): TimedTask[A] = TimedTask((_, _) => Task.now(value))
  implicit final def fromTask[A](task: Task[A]): TimedTask[A] =
    TimedTask((_, _) => task)
  final def fromTask[A](task: Task[A], timeout: Option[FiniteDuration] = None): TimedTask[A] =
    TimedTask((_, _) => task, timeout)

}

trait TaskTypes {
  type _task[R] = |=[TimedTask, R]
  type _Task[R] = <=[TimedTask, R]
}

trait TaskCreation extends TaskTypes {

  final def taskWithExecutors[R: _task, A](c: (ScheduledExecutorService, ExecutionContext) => Task[A],
                                           timeout: Option[FiniteDuration] = None): Eff[R, A] =
    Eff.send[TimedTask, R, A](TimedTask(c, timeout))

  final def task[R: _task, A](task: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => task, timeout).send[R]

  final def taskFailed[R: _task, A](t: Throwable): Eff[R, A] =
    TimedTask((_, _) => Task.fromDisjunction[Throwable, A](-\/(t))).send[R]

  final def taskSuspend[R: _task, A](task: => Task[Eff[R, A]], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => Task.suspend(task), timeout).send[R].flatten

  final def taskDelay[R: _task, A](call: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => Task.delay(call), timeout).send[R]

  final def taskForkStrategy[R: _task, A](call: Task[A], executorService: ExecutorService, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => Task.fork(call)(executorService), timeout).send

  final def taskFork[R: _task, A](call: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => Task.fork(call), timeout).send

  final def async[R: _task, A](callbackConsumer: ((Throwable Either A) => Unit) => Unit,
                               timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => Task.async[A] { cb =>
      callbackConsumer(tea => cb(\/.fromEither(tea)))
    }, timeout).send[R]

}

object TaskCreation extends TaskTypes

trait TaskInterpretation extends TaskTypes {

  def attempt[A](task: TimedTask[A]): TimedTask[Throwable Either A] = {
    TimedTask(task = (sexs, ec) => task.runNow(sexs, ec).attempt.map(_.toEither))
  }

  def taskAttempt[R, A](e: Eff[R, A])(implicit async: TimedTask /= R): Eff[R, Throwable Either A] = {
    e match {
      case Pure(a, last) =>
        Eff.pure[R, Throwable Either A](Either.right(a)).addLast(last)

      case Impure(u, c, last) =>
        async.extract(u) match {
          case Some(tx) =>
            val union = async.inject(attempt(tx))

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
          async.extract(u) match {
            case Some(tx) => async.inject(attempt(tx).asInstanceOf[TimedTask[Any]])
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
            case Left(t) => Eff.pure(Either.left(t))
            case Right(anys) => taskAttempt(continuation(anys))
          }
        }

        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  def memoize[A](key: AnyRef, cache: Cache, task: TimedTask[A]): TimedTask[A] =
    TimedTask((sexs, ec) => Task.suspend {
      cache.get(key).fold(task.runNow(sexs, ec).map { r => cache.put(key, r); r })(Task.now)
    })


  /**
    * Memoize tasks using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  def taskMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit async: TimedTask /= R): Eff[R, A] = {
    e match {
      case Pure(a, last) =>
        Pure(a, last)

      case Impure(u, c, last) =>
        async.extract(u) match {
          case Some(tx) => Impure(async.inject(memoize(key, cache, tx)), Arrs.singleton((x: u.X) => taskMemo(key, cache, c(x))), last)
          case None => Impure(u, Arrs.singleton((x: u.X) => taskMemo(key, cache, c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          async.extract(u) match {
            case Some(tx) => async.inject(memoize(key, cache, tx))
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val continuation1 = Arrs.singleton[R, List[Any], A]((ls: List[Any]) => taskMemo(key, cache, continuation(ls)))
        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  implicit final def toTaskOps[R, A](e: Eff[R, A]): TaskOps[R, A] = new TaskOps[R, A](e)

  def runTaskMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], async: TimedTask |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => TaskEffect.taskDelay[U, X](cache.memo(key, value()))
          case GetCache()        => TaskEffect.taskDelay[U, Cache](cache)
        }
    })
  }

}

final class TaskOps[R, A](val e: Eff[R, A]) extends AnyVal {

  def runTaskMemo[U](cache: Cache)(implicit m: Member.Aux[Memoized, R, U], task: TimedTask |= U): Eff[U, A] =
    TaskEffect.runTaskMemo(cache)(e)

  def taskAttempt(implicit task: TimedTask /= R): Eff[R, Throwable Either A] =
    TaskInterpretation.taskAttempt(e)

  def taskMemo(key: AnyRef, cache: Cache)(implicit async: TimedTask /= R): Eff[R, A] =
    TaskInterpretation.taskMemo(key, cache, e)
}

object TaskInterpretation extends TaskInterpretation

trait TaskEffect extends TaskInterpretation with TaskCreation

object TaskEffect extends TaskEffect

