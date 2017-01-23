package org.atnos.eff.addon.fs2

import java.util.concurrent.ScheduledExecutorService

import cats._
import cats.implicits._
import fs2._
import org.atnos.eff._
import org.atnos.eff.syntax.all._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, TimeoutException}
import scala.util._

case class TimedTask[A](task: (Strategy, Scheduler) => Task[A], timeout: Option[FiniteDuration] = None) {
  def runNow(implicit strategy: Strategy, scheduler: Scheduler): Task[A] = timeout.fold(task(strategy, scheduler)) { t =>
    for {
      ref <- Task.ref[A]
      _ <- ref.set(
        (Task.fail(new TimeoutException).schedule(t): Task[A])
          .race(task(strategy, scheduler))
          .map(_.merge)
      )
      result <- ref.get
    } yield result
  }
}

object TimedTask {
  final def TimedTaskApplicative: Applicative[TimedTask] = new Applicative[TimedTask] {
    def pure[A](x: A) = TimedTask((_, _) => Task.now(x))

    def ap[A, B](ff: TimedTask[(A) => B])(fa: TimedTask[A]) =
      TimedTask((strategy, scheduler) =>
        for {
          fStarted <- Task.start(ff.runNow(strategy, scheduler))(strategy)
          aStarted <- Task.start(fa.runNow(strategy, scheduler))(strategy)
          fDone <- fStarted
          aDone <- aStarted
        } yield fDone(aDone)
      )
  }

  implicit final def TimedTaskMonad: Monad[TimedTask] = new Monad[TimedTask] {
    def pure[A](x: A) = TimedTask((_, _) => Task.now(x))

    def flatMap[A, B](fa: TimedTask[A])(f: (A) => TimedTask[B]) =
      TimedTask((strategy, scheduler) => fa.runNow(strategy, scheduler).flatMap(f(_).runNow(strategy, scheduler)))

    def tailRecM[A, B](a: A)(f: (A) => TimedTask[Either[A, B]]): TimedTask[B] =

      TimedTask[B]({ (strategy, scheduler) =>
        def loop(na: A): Task[B] = f(na).runNow(strategy, scheduler).flatMap(_.fold(loop, Task.now))
        loop(a)
      })
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

  final def taskWithContext[R :_task, A](c: (Strategy, Scheduler) => Task[A],
                                         timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask(c, timeout).send

  final def fromTask[R :_task, A](task: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask((_, _) => task, timeout).send[R]

  final def taskFailed[R :_task, A](t: Throwable): Eff[R, A] =
    fromTask[R, A](Task.fail(t))

  final def taskSuspend[R :_task, A](tisk: => TimedTask[Eff[R, A]]): Eff[R, A] =
    TimedTask((strategy, scheduler) => Task.suspend(tisk.runNow(strategy, scheduler))).send.flatten

  final def taskDelay[R :_task, A](call: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    fromTask[R, A](Task.delay(call), timeout)

  final def taskForkStrategy[R :_task, A](call: TimedTask[A])(implicit strategy: Strategy): Eff[R, A] =
    TimedTask[A]((_, scheduler) => Task.start(call.runNow(strategy, scheduler)).flatMap(identity)).send

  final def taskFork[R :_task, A](call: TimedTask[A]): Eff[R, A] =
    TimedTask[A]((strategy, scheduler) => Task.start(call.runNow(strategy, scheduler))(strategy).flatMap(identity)).send[R]

  final def taskAsync[R :_task, A](callbackConsumer: ((Throwable Either A) => Unit) => Unit,
                                   timeout: Option[FiniteDuration] = None): Eff[R, A] =
    TimedTask[A]((strategy, _) => Task.async[A](callbackConsumer)(strategy), timeout).send[R]

  final def taskAsyncStrategy[R :_task, A](callbackConsumer: ((Throwable Either A) => Unit) => Unit,
                                           strategy: Strategy,
                                           timeout: Option[FiniteDuration] = None): Eff[R, A] =
    fromTask(Task.async[A](callbackConsumer)(strategy), timeout)

}

object TaskCreation extends TaskCreation

trait TaskInterpretation extends TaskTypes {

  def runAsync[A](e: Eff[Fx.fx1[TimedTask], A])(implicit strat: Strategy, sched: Scheduler): Task[A] =
    Eff.detachA(e)(TimedTask.TimedTaskMonad, TimedTask.TimedTaskApplicative).runNow(strat, sched)

  def runSequential[A](e: Eff[Fx.fx1[TimedTask], A])(implicit strat: Strategy, sched: Scheduler): Task[A] =
    Eff.detach(e).runNow(strat, sched)

  def attempt[A](task: TimedTask[A]): TimedTask[Throwable Either A] =
    TimedTask[Throwable Either A](task.runNow(_, _).attempt)

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
          case Some(tx) => Impure(task.inject(tx.copy(task = (strategy, scheduler) => memoize(key, cache, tx.task(strategy, scheduler)))), Arrs.singleton((x: u.X) => taskMemo(key, cache, c(x))), last)
          case None => Impure(u, Arrs.singleton((x: u.X) => taskMemo(key, cache, c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          task.extract(u) match {
            case Some(tx) => task.inject(tx.copy(task = (strategy, scheduler) => memoize(key, cache, tx.task(strategy, scheduler))))
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

  def runTaskMemo[U](cache: Cache)(implicit m: Member.Aux[Memoized, R, U], task: TimedTask |= U): Eff[U, A] =
    TaskEffect.runTaskMemo(cache)(e)

  def taskAttempt(implicit task: TimedTask /= R): Eff[R, Throwable Either A] =
    TaskInterpretation.taskAttempt(e)

  def taskMemo(key: AnyRef, cache: Cache)(implicit task: TimedTask /= R): Eff[R, A] =
    TaskInterpretation.taskMemo(key, cache, e)

  def runAsync(implicit strat: Strategy, sched: Scheduler, ev: Eff[R, A] =:= Eff[Fx.fx1[TimedTask], A]): Task[A] =
    TaskInterpretation.runAsync(e)

  def runSequential(implicit strat: Strategy, sched: Scheduler, ev: Eff[R, A] =:= Eff[Fx.fx1[TimedTask], A]): Task[A] =
    TaskInterpretation.runSequential(e)
}

object TaskInterpretation extends TaskInterpretation

trait TaskEffect extends TaskInterpretation with TaskCreation

object TaskEffect extends TaskEffect
