package org.atnos.eff.addon.monix

import cats._
import cats.effect._
import cats.implicits._
import monix.eval._
import monix.execution._
import org.atnos.eff._
import org.atnos.eff.syntax.all._

import scala.concurrent.duration.FiniteDuration
import scala.util._

trait TaskTypes {
  type _task[R] = |=[Task, R]
  type _Task[R] = <=[Task, R]
}

trait TaskCreation extends TaskTypes {

  final def fromTask[R :_task, A](task: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    timeout.fold(task)(t => task.timeout(t)).send[R]

  final def taskFailed[R :_task, A](t: Throwable): Eff[R, A] =
    fromTask(Task.fromTry[A](Failure(t)))

  final def taskSuspend[R :_task, A](task: =>Task[Eff[R, A]], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    fromTask(Task.suspend(task), timeout).flatten

  final def taskDelay[R :_task, A](call: => A, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    fromTask(Task.delay(call), timeout)

  final def taskForkScheduler[R :_task, A](call: Task[A], scheduler: Scheduler, timeout: Option[FiniteDuration] = None): Eff[R, A] =
    fromTask(call.executeOn(scheduler), timeout)

  final def taskFork[R :_task, A](call: Task[A], timeout: Option[FiniteDuration] = None): Eff[R, A] =
    fromTask(call.executeAsync, timeout)

  final def asyncBoundary[R :_task]: Eff[R, Unit] =
    fromTask(forkedUnit)

  final def asyncBoundary[R :_task](s: Scheduler): Eff[R, Unit] =
    fromTask(forkedUnit.executeOn(s))

  private val forkedUnit: Task[Unit] =
    Task.unit.executeAsync

  final def taskAsync[R :_task, A](callbackConsumer: ((Throwable Either A) => Unit) => Unit,
                                   timeout: Option[FiniteDuration] = None): Eff[R, A] = {
    val async = Task.async[A] { (_, cb) =>
      callbackConsumer(tea => cb(tea.fold(Failure(_), Success(_))))
      Cancelable.empty
    }
    fromTask(async, timeout)
  }

  def retryUntil[R :_task, A](e: Eff[R, A], condition: A => Boolean, durations: List[FiniteDuration]): Eff[R, A] =
    Eff.retryUntil(e, condition, durations, d => waitFor(d))

  def waitFor[R :_task](duration: FiniteDuration): Eff[R, Unit] =
    Eff.send(Task.deferAction(scheduler => Task.delay { scheduler.scheduleOnce(duration)(()); () }))
}

object TaskCreation extends TaskCreation

trait TaskInterpretation extends TaskTypes {

  private val monixTaskMonad: MonadError[Task, Throwable] =
    MonadError[Task, Throwable]

  private val monixTaskApplicative : Applicative[Task] =
    Task.catsParallel.applicative.asInstanceOf[Applicative[Task]]

  def runAsync[R, A](e: Eff[R, A])(implicit m: Member.Aux[Task, R, NoFx]): Task[A] =
    Eff.detachA(e)(monixTaskMonad, monixTaskApplicative, m)

  def runSequential[R, A](e: Eff[R, A])(implicit m: Member.Aux[Task, R, NoFx]): Task[A] =
    Eff.detach(e)(monixTaskMonad, m)

  import interpret.of

  def taskAttempt[R, A](e: Eff[R, A])(implicit task: Task /= R): Eff[R, Throwable Either A] =
    interpret.interceptNatM[R, Task, Throwable Either ?, A](e,
      new (Task ~> (Task of (Throwable Either ?))#l) {
        def apply[X](fa: Task[X]): Task[Throwable Either X] =
          fa.attempt
      })

  def forkTasks[R, A](e: Eff[R, A])(implicit task: Task /= R): Eff[R, A] =
    interpret.interceptNat[R, Task, A](e)(
      new (Task ~> Task) {
        def apply[X](fa: Task[X]): Task[X] =
          fa.executeAsync
      })

  /** memoize the task result using a cache */
  def memoize[A](key: AnyRef, cache: Cache, task: Task[A]): Task[A] =
    Task.suspend {
      cache.get[A](key).fold(task.map { r => cache.put(key, r); r })(Task.now)
    }

  /**
    * Memoize task effects using a cache
    *
    * if this method is called with the same key the previous value will be returned
    */
  def taskMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit task: Task /= R): Eff[R, A] =
    taskAttempt(Eff.memoizeEffect(e, cache, key)).flatMap {
      case Left(t)  => Eff.send(taskSequenceCached.reset(cache, key)) >> TaskEffect.taskFailed(t)
      case Right(a) => Eff.pure(a)
    }

  /**
    * Memoize task values using a memoization effect
    *
    * if this method is called with the same key the previous value will be returned
    */
  def taskMemoized[R, A](key: AnyRef, e: Eff[R, A])(implicit task: Task /= R, m: Memoized |= R): Eff[R, A] =
    MemoEffect.getCache[R].flatMap(cache => taskMemo(key, cache, e))

  def runTaskMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], task: Task |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => TaskCreation.taskDelay(cache.memo(key, value()))
          case GetCache()        => TaskCreation.taskDelay(cache)
        }
    })
  }

  implicit val taskSequenceCached: SequenceCached[Task] = new SequenceCached[Task] {
    def get[X](cache: Cache, key: AnyRef): Task[Option[X]] =
      Task.delay(cache.get(key)).executeAsync

    def apply[X](cache: Cache, key: AnyRef, sequenceKey: Int, tx: =>Task[X]): Task[X] =
      cache.memo((key, sequenceKey), tx.memoize)

    def reset(cache: Cache, key: AnyRef): Task[Unit] =
      Task.delay {
        cache.reset(key)
        var i = 0
        while (cache.get((key, i)).isDefined) {
          cache.reset((key, i))
          i += 1
        }
      }
  }

}

object TaskInterpretation extends TaskInterpretation

trait EffToTask[R] {
  def apply[A](e: Eff[R, A]): Task[A]
}

trait TaskEffect extends TaskInterpretation with TaskCreation { outer =>

  implicit def asyncInstance[R :_Task]: cats.effect.Async[Eff[R, ?]] = new cats.effect.Async[Eff[R, ?]] {

    private val taskAsyncInstance: cats.effect.Async[Task] =
      implicitly[cats.effect.Async[Task]]

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): Eff[R, A] =
      fromTask(taskAsyncInstance.async(k))

    def suspend[A](thunk: =>Eff[R, A]): Eff[R, A] =
      fromTask(Task.apply(thunk)).flatten

    def raiseError[A](e: Throwable): Eff[R, A] =
      fromTask(taskAsyncInstance.raiseError(e))

    def handleErrorWith[A](fa: Eff[R, A])(f: Throwable => Eff[R, A]): Eff[R, A] =
      taskAttempt(fa).flatMap {
        case Left(t)  => f(t)
        case Right(a) => Eff.pure(a)
      }

    def pure[A](a: A): Eff[R,A] =
      Eff.pure(a)

    def flatMap[A, B](fa: Eff[R,A])(f: A =>Eff[R, B]): Eff[R, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Eff[R, Either[A, B]]): Eff[R, B] =
      Eff.EffMonad[R].tailRecM(a)(f)

  }

  def effectInstance[R :_Task](implicit runEff: EffToTask[R], scheduler: Scheduler): cats.effect.Effect[Eff[R, ?]] = new cats.effect.Effect[Eff[R, ?]] {

    private val taskEffectInstance: cats.effect.Effect[Task] =
      implicitly[cats.effect.Effect[Task]]

    private val asyncInstance: cats.effect.Async[Eff[R, ?]] =
      outer.asyncInstance

    def runAsync[A](fa: Eff[R, A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] =
      taskEffectInstance.runAsync(runEff(fa))(cb)

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): Eff[R, A] =
      asyncInstance.async(k)

    def suspend[A](thunk: =>Eff[R, A]): Eff[R, A] =
      asyncInstance.suspend(thunk)

    def raiseError[A](e: Throwable): Eff[R, A] =
      asyncInstance.raiseError(e)

    def handleErrorWith[A](fa: Eff[R, A])(f: Throwable => Eff[R, A]): Eff[R, A] =
      asyncInstance.handleErrorWith(fa)(f)

    def pure[A](a: A): Eff[R,A] =
      Eff.pure(a)

    def flatMap[A, B](fa: Eff[R,A])(f: A =>Eff[R, B]): Eff[R, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Eff[R, Either[A, B]]): Eff[R, B] =
      Eff.EffMonad[R].tailRecM(a)(f)
  }

}

object TaskEffect extends TaskEffect
