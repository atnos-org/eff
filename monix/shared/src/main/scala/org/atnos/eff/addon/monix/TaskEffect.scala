package org.atnos.eff.addon.monix

import cats._
import cats.effect._
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
    val async = Task.create[A] { (_, cb) =>
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

  private val monixTaskApplicative = new Applicative[Task] {
    override def ap[A, B](ff: Task[(A) => B])(fa: Task[A]): Task[B] = Task.mapBoth(ff, fa)(_ (_))

    override def map2[A, B, Z](fa: Task[A], fb: Task[B])(f: (A, B) => Z): Task[Z] = Task.mapBoth(fa, fb)(f)

    override def product[A, B](fa: Task[A], fb: Task[B]): Task[(A, B)] = Task.mapBoth(fa, fb)((_, _))

    override def pure[A](a: A): Task[A] = Task.now(a)

    override val unit: Task[Unit] = Task.unit

    override def map[A, B](fa: Task[A])(f: (A) => B): Task[B] = fa.map(f)
  }

  def runAsync[R, A](e: Eff[R, A])(implicit m: Member.Aux[Task, R, NoFx]): Task[A] =
    Eff.detachA(e)(monixTaskMonad, monixTaskApplicative, m)

  def runSequential[R, A](e: Eff[R, A])(implicit m: Member.Aux[Task, R, NoFx]): Task[A] =
    Eff.detach(e)(monixTaskMonad, m)

  import interpret.of

  def taskAttempt[R, A](e: Eff[R, A])(implicit task: Task /= R): Eff[R, Throwable Either A] =
    interpret.interceptNatM[R, Task, Either[Throwable, *], A](e,
      new (Task ~> (Task of Either[Throwable, *])#l) {
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

  implicit def asyncInstance[R :_Task](implicit runEff: EffToTask[R]): cats.effect.Async[Eff[R, *]] = new cats.effect.Async[Eff[R, *]] {
    private val taskAsyncInstance: cats.effect.Async[Task] =
      implicitly[cats.effect.Async[Task]]

    override def asyncF[A](k: (Either[Throwable, A] => Unit) => Eff[R, Unit]): Eff[R, A] = fromTask(taskAsyncInstance.asyncF[A] { f => runEff(k(f)) })

    override def bracketCase[A, B](acquire: Eff[R, A])(use: A => Eff[R, B])(release: (A, ExitCase[Throwable]) => Eff[R, Unit]): Eff[R, B] =
      fromTask(taskAsyncInstance.bracketCase(runEff(acquire))(a => runEff(use(a)))((r, ec) => runEff(release(r, ec))))

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): Eff[R, A] =
      fromTask(taskAsyncInstance.async_(k))

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

  def effectInstance[R :_Task](implicit runEff: EffToTask[R], scheduler: Scheduler): cats.effect.Effect[Eff[R, *]] = new cats.effect.Effect[Eff[R, *]] {

    private val taskEffectInstance: cats.effect.Effect[Task] =
      implicitly[cats.effect.Effect[Task]]

    private val asyncInstance: cats.effect.Async[Eff[R, *]] =
      outer.asyncInstance

    override def asyncF[A](k: (Either[Throwable, A] => Unit) => Eff[R, Unit]) = asyncInstance.asyncF(k)

    override def bracketCase[A, B](acquire: Eff[R, A])(use: A => Eff[R, B])(release: (A, ExitCase[Throwable]) => Eff[R, Unit]) =
      asyncInstance.bracketCase(acquire)(use)(release)

    def runAsync[A](fa: Eff[R, A])(cb: Either[Throwable, A] => IO[Unit]): SyncIO[Unit] =
      taskEffectInstance.runAsync(runEff(fa))(cb)

    def async[A](k: (Either[Throwable, A] => Unit) => Unit): Eff[R, A] =
      asyncInstance.async_(k)

    def suspend[A](thunk: =>Eff[R, A]): Eff[R, A] =
      asyncInstance.defer(thunk)

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
