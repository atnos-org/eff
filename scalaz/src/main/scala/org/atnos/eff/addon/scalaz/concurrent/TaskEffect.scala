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
import scala.util.{Either, Failure, Success}

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

  implicit val timedTaskSequenceCached: SequenceCached[TimedTask] = new SequenceCached[TimedTask] {
    def apply[X](cache: Cache, key: AnyRef, sequenceKey: Int, tx: =>TimedTask[X]): TimedTask[X] = TimedTask { (sexs, ec) =>
      implicit val executionContext = ec
      // there is no built-in memoization for Scalaz tasks so we need to memoize future instead
      lazy val cached = cache.memo((key, sequenceKey), taskToFuture(tx.runNow(sexs, ec)))

      Task async { cb =>
        cached.onComplete {
          case Success(a) => cb(\/-(a))
          case Failure(t) => cb(-\/(t))
        }
      }
    }
  }

  private[concurrent] def futureToTask[A](future: =>scala.concurrent.Future[A])(implicit ec: ExecutionContext, S: Strategy): Task[A] =
    Task async { cb =>
      future onComplete {
        case Success(a) => S { cb(\/-(a)) }
        case Failure(t) => S { cb(-\/(t)) }
      }
    }

  private[concurrent] def taskToFuture[A](task: Task[A]): scala.concurrent.Future[A] = {
    val p = Promise[A]()

    task unsafePerformAsync {
      case \/-(a) => p.complete(Success(a)); ()
      case -\/(t) => p.complete(Failure(t)); ()
    }

    p.future
  }


}

trait TaskTypes {
  type _task[R] = |=[TimedTask, R]
  type _Task[R] = <=[TimedTask, R]
}

trait TaskCreation extends TaskTypes {

  final def taskWithExecutors[R :_task, A](c: (ScheduledExecutorService, ExecutionContext) => Task[A],
                                           timeout: Option[FiniteDuration] = None): Eff[R, A] =
    Eff.send[TimedTask, R, A](TimedTask(c, timeout))

  final def fromTask[R :_task, A](task: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => task, timeout).send[R]

  final def taskFailed[R :_task, A](t: Throwable): Eff[R, A] =
    TimedTask((_, _) => Task.fromDisjunction[Throwable, A](-\/(t))).send[R]

  final def taskSuspend[R :_task, A](task: => Task[Eff[R, A]], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => Task.suspend(task), timeout).send[R].flatten

  final def taskDelay[R :_task, A](call: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => Task.delay(call), timeout).send[R]

  final def taskForkStrategy[R :_task, A](call: Task[A], executorService: ExecutorService, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => Task.fork(call)(executorService), timeout).send

  final def taskFork[R :_task, A](call: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => Task.fork(call), timeout).send

  final def async[R :_task, A](callbackConsumer: ((Throwable Either A) => Unit) => Unit,
                               timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => Task.async[A] { cb =>
      callbackConsumer(tea => cb(\/.fromEither(tea)))
    }, timeout).send[R]

}

object TaskCreation extends TaskTypes

trait TaskInterpretation extends TaskTypes {

  def runAsync[A](e: Eff[Fx.fx1[TimedTask], A])(implicit sexs: ScheduledExecutorService, ec: ExecutionContext): Task[A] =
    Eff.detachA(e)(TimedTask.TimedTaskMonad, TimedTask.TimedTaskApplicative).runNow(sexs, ec)

  def runSequential[A](e: Eff[Fx.fx1[TimedTask], A])(implicit sexs: ScheduledExecutorService, ec: ExecutionContext): Task[A] =
    Eff.detach(e).runNow(sexs, ec)

  def attempt[A](task: TimedTask[A]): TimedTask[Throwable Either A] = {
    TimedTask(task = (sexs, ec) => task.runNow(sexs, ec).attempt.map(_.toEither))
  }

  import interpret.of

  def taskAttempt[R, A](e: Eff[R, A])(implicit async: TimedTask /= R): Eff[R, Throwable Either A] =
    interpret.interceptNatM[R, TimedTask, Throwable Either ?, A](e,
      new (TimedTask ~> (TimedTask of (Throwable Either ?))#l) {
        override def apply[X](fa: TimedTask[X]): TimedTask[Throwable Either X] = attempt(fa)
      })

  /** memoize the task result */
  def memoize[A](key: AnyRef, cache: Cache, task: TimedTask[A]): TimedTask[A] =
    TimedTask((sexs, ec) => Task.suspend {
      cache.get(key).fold(task.runNow(sexs, ec).map { r => cache.put(key, r); r })(Task.now)
    })


  /**
    * Memoize task effects using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  def taskMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit task: TimedTask /= R): Eff[R, A] =
    Eff.memoizeEffect(e, cache, key)

  def runTaskMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], task: TimedTask |= U): Eff[U, A] =
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => TaskEffect.taskDelay[U, X](cache.memo(key, value()))
          case GetCache()        => TaskEffect.taskDelay[U, Cache](cache)
        }
    })
}

object TaskInterpretation extends TaskInterpretation

trait TaskEffect extends TaskInterpretation with TaskCreation

object TaskEffect extends TaskEffect

