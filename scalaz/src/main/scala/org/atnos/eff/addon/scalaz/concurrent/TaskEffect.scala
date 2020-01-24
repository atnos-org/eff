package org.atnos.eff.addon.scalaz.concurrent

import java.util.concurrent._

import org.atnos.eff.syntax.all._

import scalaz.{-\/, Nondeterminism, \/, \/-}
import scalaz.concurrent._
import cats._
import cats.implicits._
import org.atnos.eff._

import scala.concurrent.{ExecutionContext, Promise, TimeoutException}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Either, Failure, Success}

case class TimedTask[A](run: ExecutorServices => Task[A], timeout: Option[FiniteDuration] = None) {

  def runNow(es: ExecutorServices): Task[A] =
    timeout.fold(run(es)) { t =>
      Task.async[A] { register =>
        val promise = Promise[A]
        val cancelTimeout = es.scheduler.schedule({ val _ = promise.tryFailure(new TimeoutException) }, t)

        run(es).unsafePerformAsync { tea =>
          promise.tryComplete(tea.fold(Failure(_), Success(_)))
          cancelTimeout()
        }
        promise.future.onComplete(t => register(if (t.isSuccess) \/-(t.get) else -\/(t.failed.get)))(es.executionContext)
      }
  }

}

object TimedTask {

  def TimedTaskApplicative: Applicative[TimedTask] = new Applicative[TimedTask] {
    def pure[A](x: A): TimedTask[A] =
      TimedTask(_ => Task.now(x))

    def ap[A, B](ff: TimedTask[A => B])(fa: TimedTask[A]): TimedTask[B] =
      TimedTask[B] { ess =>
        Nondeterminism[Task].mapBoth(Task.suspend(ff.runNow(ess)), Task.suspend(fa.runNow(ess)))((f, a) => f(a))
      }

    override def toString = "Applicative[Task]"
  }

  implicit final val TimedTaskMonad: MonadError[TimedTask, Throwable] = new MonadError[TimedTask, Throwable] {
    def pure[A](x: A): TimedTask[A] =
      TimedTask(_ => Task.now(x))

    def flatMap[A, B](fa: TimedTask[A])(f: A => TimedTask[B]): TimedTask[B] =
      TimedTask(es => fa.runNow(es).flatMap(f(_).runNow(es)))

    def tailRecM[A, B](a: A)(f: A => TimedTask[Either[A, B]]): TimedTask[B] =
      TimedTask { es =>
        def loop(na: A): Task[B] = { f(na).runNow(es).flatMap(_.fold(loop, Task.now)) }
        loop(a)
      }

    def raiseError[A](e: Throwable): TimedTask[A] =
      TimedTask(_ => Task.fail(e))

    def handleErrorWith[A](fa: TimedTask[A])(f: Throwable => TimedTask[A]): TimedTask[A] =
      TimedTask(ess => fa.runNow(ess).handleWith[A] { case t => f(t).runNow(ess) })

    override def toString = "MonadError[Task, Throwable]"

  }

  final def now[A](value: A): TimedTask[A] =
    TimedTask(_ => Task.now(value))

  final def delay[A](value: =>A): TimedTask[A] =
    TimedTask(_ => Task.delay(value))

  implicit final def fromTask[A](task: Task[A]): TimedTask[A] =
    TimedTask(_ => task)

  final def fromTask[A](task: Task[A], timeout: Option[FiniteDuration] = None): TimedTask[A] =
    TimedTask(_ => task, timeout)

  implicit val timedTaskSequenceCached: SequenceCached[TimedTask] = new SequenceCached[TimedTask] {
    def get[X](cache: Cache, key: AnyRef): TimedTask[Option[X]] =
      TimedTask { ess =>
        val es = ess.executorService
        Task.fork(Task.delay(cache.get(key)))(es)
      }

    def apply[X](cache: Cache, key: AnyRef, sequenceKey: Int, tx: =>TimedTask[X]): TimedTask[X] = {
      TimedTask { es =>
        implicit val executionContext = es.executionContext
        // there is no built-in memoization for Scalaz tasks so we need to memoize future instead
        lazy val cached = cache.memo((key, sequenceKey), taskToFuture(tx.runNow(es)))

        Task async { cb =>
          cached.onComplete {
            case Success(a) => cb(\/-(a))
            case Failure(t) => cb(-\/(t))
          }
        }
      }
    }

    def reset(cache: Cache, key: AnyRef): TimedTask[Unit] =
      TimedTask(_ => Task.now {
        cache.reset(key)
        var i = 0
        while (cache.get((key, i)).isDefined) {
          cache.reset((key, i))
          i += 1
        }
      })
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

  final def taskWithExecutors[R :_task, A](run: ExecutorServices => Task[A],
                                           timeout: Option[FiniteDuration] = None): Eff[R, A] =
    Eff.send[TimedTask, R, A](TimedTask(run, timeout))

  final def fromTask[R :_task, A](task: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => task, timeout).send[R]

  final def taskFailed[R :_task, A](t: Throwable): Eff[R, A] =
    TimedTask(_ => Task.fromDisjunction[Throwable, A](-\/(t))).send[R]

  final def taskSuspend[R :_task, A](task: =>Task[Eff[R, A]], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.suspend(task), timeout).send[R].flatten

  final def taskDelay[R :_task, A](call: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.delay(call), timeout).send[R]

  /** use a specific executor service to run the task */
  final def taskFork[R :_task, A](call: Task[A], executorService: ExecutorService, timeout: FiniteDuration): Eff[R, A] =
    taskForkWithExecutorServiceAndTimeout(call, executorService, Some(timeout))

  /** use a specific executor service to run the task */
  final def taskFork[R :_task, A](call: Task[A], executorService: ExecutorService): Eff[R, A] =
    taskForkWithExecutorServiceAndTimeout(call, executorService, None)

  final def taskForkWithExecutorServiceAndTimeout[R :_task, A](call: Task[A], executorService: ExecutorService, timeout: Option[FiniteDuration]): Eff[R, A] =
    TimedTask(_ => Task.fork(call)(executorService), timeout).send

  final def taskFork[R :_task, A](call: Task[A], timeout: FiniteDuration): Eff[R, A] =
    taskForkWithTimeout(call, Some(timeout))

  final def taskFork[R :_task, A](call: Task[A]): Eff[R, A] =
    taskForkWithTimeout(call, None)

  final def taskForkWithTimeout[R :_task, A](call: Task[A], timeout: Option[FiniteDuration]): Eff[R, A] =
    TimedTask(es => Task.fork(call)(es.executorService), timeout).send


  final def async[R :_task, A](callbackConsumer: ((Throwable Either A) => Unit) => Unit,
                               timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(_ => Task.async[A] { cb =>
      callbackConsumer(tea => cb(\/.fromEither(tea)))
    }, timeout).send[R]

  def retryUntil[R :_task, A](e: Eff[R, A], condition: A => Boolean, durations: List[FiniteDuration]): Eff[R, A] =
    Eff.retryUntil(e, condition, durations, d => waitFor(d))

  def waitFor[R :_task](duration: FiniteDuration): Eff[R, Unit] =
    Eff.send(TimedTask(_ => Task.now(()).after(duration)))

}

object TaskCreation extends TaskCreation

trait TaskInterpretation extends TaskTypes {

  def runAsync[R, A](e: Eff[R, A])(implicit es: ExecutorServices, m: Member.Aux[TimedTask, R, NoFx]): Task[A] =
    Eff.detachA(e)(TimedTask.TimedTaskMonad, TimedTask.TimedTaskApplicative, m).runNow(es)

  def runSequential[R, A](e: Eff[R, A])(implicit es: ExecutorServices, m: Member.Aux[TimedTask, R, NoFx]): Task[A] =
    Eff.detach(e)(TimedTask.TimedTaskMonad, m).runNow(es)

  def attempt[A](task: TimedTask[A]): TimedTask[Throwable Either A] = {
    TimedTask(es => task.runNow(es).attempt.map(_.toEither))
  }

  import interpret.of

  def taskAttempt[R, A](e: Eff[R, A])(implicit async: TimedTask /= R): Eff[R, Throwable Either A] =
    interpret.interceptNatM[R, TimedTask, Throwable Either *, A](e,
      new (TimedTask ~> (TimedTask of (Throwable Either *))#l) {
        override def apply[X](fa: TimedTask[X]): TimedTask[Throwable Either X] = attempt(fa)
      })

  /** memoize the task result */
  def memoize[A](key: AnyRef, cache: Cache, task: TimedTask[A]): TimedTask[A] =
    TimedTask(es => Task.suspend {
      cache.get(key).fold(task.runNow(es).map { r => cache.put(key, r); r })(Task.now)
    })


  /**
    * Memoize task effects using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  def taskMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit task: TimedTask /= R): Eff[R, A] =
    taskAttempt(Eff.memoizeEffect(e, cache, key)).flatMap {
      case Left(t)  => Eff.send(TimedTask.timedTaskSequenceCached.reset(cache, key)) >> TaskEffect.taskFailed(t)
      case Right(a) => Eff.pure(a)
    }

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
