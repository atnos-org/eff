package org.atnos.eff

import cats._
import cats.implicits._
import eff._
import interpret._
import EitherEffect._
import org.atnos.eff.Interpret.runInterpreter

import scala.reflect.ClassTag

trait SafeEffect extends
  SafeCreation with
  SafeInterpretation

object SafeEffect extends SafeEffect

trait SafeTypes {

  type _Safe[R] = Safe <= R
  type _safe[R] = Safe |= R
}

trait SafeCreation extends SafeTypes {

  def protect[R :_safe, A](a: =>A): Eff[R, A] =
    send[Safe, R, A](Safe.evaluate(a))

  def eval[R :_safe , A](a: Eval[A]): Eff[R, A] =
    send[Safe, R, A](Safe.eval(a))

  def exception[R :_safe, A](t: Throwable): Eff[R, A] =
    send[Safe, R, A](Safe.fail(t))

  def finalizerException[R :_safe](t: Throwable): Eff[R, Unit] =
    send[Safe, R, Unit](Safe.failFinalizer(t))
}

trait SafeInterpretation extends SafeCreation { outer =>

  type Out[A] = (ThrowableEither[A], List[Throwable])

  /**
   * Run a safe effect
   *
   * Collect finalizer exceptions if any
   */
  def runSafe[R, U, A](effect: Eff[R, A])(implicit m: Member.Aux[Safe, R, U]): Eff[U, (ThrowableEither[A], List[Throwable])] =
    runInterpreter[R, U, Safe, A, Out[A]](effect)(safeInterpreter[U, A])

  /** run a safe effect but drop the finalizer errors */
  def execSafe[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Safe, R, U]): Eff[U, ThrowableEither[A]] =
    runSafe(r).map(_._1)

  /**
   * Attempt to execute a safe action including finalizers
   */
  def attemptSafe[R, A](effect: Eff[R, A])(implicit m: Safe /= R): Eff[R, (ThrowableEither[A], List[Throwable])] =
    protect(intercept[R, Safe, A, Out[A]](effect)(safeInterpreter[R, A])).flatten

  def safeInterpreter[R, A]: Interpreter[Safe, R, A, Out[A]] =
    safeInterpreter(None)

  def safeInterpreter[R, A](last: Option[(Eff[R, Unit], Safe /= R)]): Interpreter[Safe, R, A, Out[A]] = new Interpreter[Safe, R, A, Out[A]] {
    var errors: Vector[Throwable] = Vector()

    def onPure(a: A): Eff[R, Out[A]] =
      last match {
        case None => Eff.pure((Right(a), errors.toList))
        case Some((l, m)) =>
          attempt(l)(m) flatMap {
            case Left(t)   => outer.finalizerException[R](t)(m) >> pure((Right(a), errors.toList))
            case Right(_) => pure((Right(a), errors.toList))
          }
      }

    def onEffect[X](sx: Safe[X], continuation: Continuation[R, X, Out[A]]): Eff[R, Out[A]] =
      sx match {
        case EvaluateValue(v) =>
          Either.catchNonFatal(v.value) match {
            case Left(e) =>
              continuation.runOnNone >> {
                last match {
                  case None =>
                    Eff.pure((Left(e), errors.toList))

                  case Some((l, m)) =>
                    attempt(l)(m) flatMap {
                      case Left(t)   => outer.finalizerException[R](t)(m) >> outer.exception[R, Out[A]](e)(m)
                      case Right(_) => outer.exception[R, Out[A]](e)(m)
                    }
                }
              }

            case Right(x) =>
              Eff.impure(x, continuation)
          }

        case FailedValue(t) =>
          continuation.runOnNone >> Eff.pure((Left(t), errors.toList))

        case FailedFinalizer(t) =>
          errors = errors :+ t
          continuation.runOnNone >> Eff.impure((), continuation)
      }

    def onLastEffect[X](sx: Safe[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] =
      sx match {
        case EvaluateValue(v) =>
          Either.catchNonFatal(v.value) match {
            case Left(e) =>
              last match {
                case None => Eff.pure(())
                case Some((l, m)) =>
                  attempt(l)(m) flatMap {
                    case Left(t)   => outer.finalizerException[R](t)(m) >> outer.exception[R, Unit](e)(m)
                    case Right(_) => outer.exception[R, Unit](e)(m)
                  }
              }

            case Right(x) =>
              last match {
                case None => Eff.impure(x, continuation)
                case Some((l, m)) =>
                  attempt(l)(m) flatMap {
                    case Left(t)   => outer.finalizerException[R](t)(m) >> Eff.impure(x, continuation)
                    case Right(_) => Eff.impure(x, continuation)
                  }
              }
          }

        case FailedValue(t) =>
          Eff.pure(())

        case FailedFinalizer(t) =>
          errors = errors :+ t
          Eff.impure((), continuation)
      }

    def onApplicativeEffect[X, T[_] : Traverse](xs: T[Safe[X]], continuation: Continuation[R, T[X], Out[A]]): Eff[R, Out[A]] = {
      val failedFinalizers = new collection.mutable.ListBuffer[Throwable]
      var error: Option[Throwable] = None

      val traversed: T[X] = xs.map {
        case FailedFinalizer(t) => failedFinalizers.append(t); ().asInstanceOf[X]
        case FailedValue(t)     => error = Some(t); ().asInstanceOf[X]
        case EvaluateValue(v)   =>
          error match {
            case None =>
              Either.catchNonFatal(v.value) match {
                case Right(a) => a
                case Left(t)  => error = Some(t); ().asInstanceOf[X]
              }
            case Some(_) => ().asInstanceOf[X]
          }
      }

      errors = errors ++ failedFinalizers.toVector
      error match {
        case None =>
          Eff.impure(traversed, continuation)

        case Some(t) =>
          last match {
            case None =>
              Eff.pure((Left(t), errors.toList))

            case Some((l, m)) =>
              attempt(l)(m) flatMap {
                case Left(t1)  => outer.finalizerException[R](t1)(m) >> outer.exception[R, Out[A]](t)(m)
                case Right(_) => exception[R, Out[A]](t)(m)
              }
          }
      }
    }
  }

  /**
   * evaluate first action possibly having error effects
   * execute a second action whether the first is successful or not but keep track of finalizer exceptions
   */
  def thenFinally[R, A](effect: Eff[R, A], last: Eff[R, Unit])(implicit m: Safe /= R): Eff[R, A] =
    intercept[R, Safe, A, Out[A]](Eff.whenStopped(effect, Last.eff(last)))(safeInterpreter[R, A](Some((last, m)))).flatMap {
      case (Right(a), vs) => vs.traverse(v => outer.finalizerException(v)).void >> Eff.pure(a)
      case (Left(t), vs) => vs.traverse(v => outer.finalizerException(v)).void >> outer.exception(t)
    }

  /**
   * get a resource A and use it.
   * Call the release function whether an exception is thrown or not when using the resource
   *
   * NOTE: Eff interpreters are independent so if there is an effect short-circuiting all computations inside 'use',
   * like Option or Either then the release function will not be called. If you want to make sure
   * that the release function is always called "at the end of the world and whatever happens" you need to call
   * Eff.bracketLast
   */
  def bracket[R, A, B, C](acquire: Eff[R, A])(use: A => Eff[R, B])(release: A => Eff[R, C])(implicit m: Safe /= R): Eff[R, B] =
    for {
      a <- acquire
      b <- thenFinally(use(a), release(a).void)
    } yield b

  /**
   * evaluate first action possibly having exceptions
   *
   * Execute a second action if the first one is not successful
   */
  def otherwise[R, A](action: Eff[R, A], onThrowable: Eff[R, A])(implicit m: Safe /= R): Eff[R, A] =
    whenFailed(action, _ => onThrowable)

  /**
   * evaluate first action possibly having error effects
   *
   * Execute a second action if the first one is not successful, based on the error
   */
  def catchThrowable[R, A, B](action: Eff[R, A], pureValue: A => B, onThrowable: Throwable => Eff[R, B])(implicit m: Safe /= R): Eff[R, B] =
    recoverThrowable[R, A, B](action, pureValue, {case t => onThrowable(t)})

  /**
   * evaluate first action possibly having error effects
   *
   * Execute a second action if the first one is not successful and second is defined for the error
   */
  def recoverThrowable[R, A, B](action: Eff[R, A], pureValue: A => B, onThrowable: PartialFunction[Throwable, Eff[R, B]])(implicit m: Safe /= R): Eff[R, B] =
    attemptSafe(action).flatMap {
      case (Left(t), ls)  if onThrowable.isDefinedAt(t) => onThrowable(t).flatMap(b => ls.traverse(f => finalizerException(f)).as(b))
      case (Left(t), _) => exception(t)
      case (Right(a), ls) => pure(pureValue(a)).flatMap(b => ls.traverse(f => finalizerException(f)).as(b))
    }

  /**
   * evaluate first action possibly throwing exceptions
   *
   * Execute a second action if the first one is not successful, based on the exception
   *
   * The final value type is the same as the original type
   */
  def whenFailed[R, A](action: Eff[R, A], onThrowable: Throwable => Eff[R, A])(implicit m: Safe /= R): Eff[R, A] =
    catchThrowable(action, identity[A], onThrowable)

  /**
   * evaluate first action possibly throwing exceptions
   *
   * Execute a second action if the first one is not successful and second is defined for the error
   *
   * The final value type is the same as the original type
   */
  def whenThrowable[R, A](action: Eff[R, A], onThrowable: PartialFunction[Throwable, Eff[R, A]])(implicit m: Safe /= R): Eff[R, A] =
    recoverThrowable(action, identity[A], onThrowable)

  /**
   * try to execute an action an report any issue
   */
  def attempt[R, A](action: Eff[R, A])(implicit m: Safe /= R): Eff[R, Throwable Either A] =
    catchThrowable(action, Right[Throwable, A], (t: Throwable) => pure(Left(t)))

  /**
   * ignore one possible exception that could be thrown
   */
  def ignoreException[R, E <: Throwable : ClassTag, A](action: Eff[R, A])(implicit m: Safe /= R): Eff[R, Unit] =
    recoverThrowable[R, A, Unit](action, _ => (), {
      case t if implicitly[ClassTag[E]].runtimeClass.isInstance(t) => pure(())
    })

  /**
   * Memoize safe effects using a cache
   *
   * if this method is called with the same key the previous value will be returned
   */
  def safeMemo[R, A](key: AnyRef, cache: Cache, e: Eff[R, A])(implicit safe: Safe /= R): Eff[R, A] =
    attempt(Eff.memoizeEffect(e, cache, key)).flatMap {
      case Left(t)  => Eff.send(Safe.safeSequenceCached.reset(cache, key)) >> SafeEffect.exception(t)
      case Right(a) => Eff.pure(a)
    }


}

object SafeInterpretation extends SafeInterpretation

/**
 * The Safe type is a mix of a ThrowableEither / Eval effect
 *   and a writer effect to collect finalizer failures
 */
sealed trait Safe[A] {
  def memoize: Safe[A]
}

case class EvaluateValue[A](run: Eval[A])  extends Safe[A] {
  def memoize: Safe[A] =
    copy(run.memoize)
}

case class FailedValue[A](t: Throwable)  extends Safe[A] {
  def memoize: Safe[A] =
    this
}

case class FailedFinalizer(t: Throwable) extends Safe[Unit] {
  def memoize: Safe[Unit] =
    this
}

object Safe {

  def evaluate[A](a: =>A): Safe[A] =
    EvaluateValue[A](Eval.later(a))

  def eval[A](a: Eval[A]): Safe[A] =
    EvaluateValue[A](a)

  def fail[A](t: Throwable): Safe[A] =
    FailedValue(t)

  def failFinalizer(t: Throwable): Safe[Unit] =
    FailedFinalizer(t)

  implicit val safeSequenceCached: SequenceCached[Safe] =
    new SequenceCached[Safe] {
      def get[X](cache: Cache, key: AnyRef): Safe[Option[X]] =
        evaluate(cache.get(key))

      def apply[X](cache: Cache, key: AnyRef, subKey: Int, tx: =>Safe[X]): Safe[X] =
        cache.memo((key, subKey), tx.memoize)

      def reset(cache: Cache, key: AnyRef): Safe[Unit] =
        EvaluateValue(Eval.later {
          cache.reset(key)
          var i = 0
          while (cache.get((key, i)).isDefined) {
            cache.reset((key, i))
            i += 1
          }
        })

    }

}
