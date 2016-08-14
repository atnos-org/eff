package org.atnos.eff

import cats._
import cats.data._
import cats.implicits._
import eff._
import interpret._
import xor._

import scala.reflect.ClassTag

trait SafeEffect extends
  SafeCreation with
  SafeInterpretation

object SafeEffect extends SafeEffect

trait SafeTypes {

  /**
   * base type for this effect: either an error or a computation to evaluate
   * a "by-name" value
   */
  sealed trait Safe[A]

  case class EvaluateValue[A](a: Eval[A])  extends Safe[A]
  case class FailedValue[A](t: Throwable)  extends Safe[A]
  case class FailedFinalizer(t: Throwable) extends Safe[Unit]

  type _Safe[R] = Safe <= R
  type _safe[R] = Safe |= R
}

trait SafeCreation extends SafeTypes {

  def ok[R :_safe, A](a: =>A): Eff[R, A] =
    send[Safe, R, A](EvaluateValue[A](Eval.later(a)))

  def eval[R :_safe , A](a: Eval[A]): Eff[R, A] =
    send[Safe, R, A](EvaluateValue[A](a))

  def exception[R :_safe, A](t: Throwable): Eff[R, A] =
    send[Safe, R, A](FailedValue(t))

  def finalizerException[R :_safe](t: Throwable): Eff[R, Unit] =
    send[Safe, R, Unit](FailedFinalizer(t))
}

trait SafeInterpretation extends SafeCreation { outer =>

  /**
   * Run an safe effect.
   *
   * Make sure all finalizers are run
   */
  def runSafe[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Safe, R, U]): Eff[U, (ThrowableXor[A], List[Throwable])] = {
    type Out = (ThrowableXor[A], Vector[Throwable])
    val loop = new Loop[Safe, R, A, Eff[U, Out]] {
      type S = Vector[Throwable]
      val init: S = Vector.empty[Throwable]

      def onPure(a: A, s: S): (Eff[R, A], S) Xor Eff[U, Out] =
        Xor.Right(pure((Xor.Right(a), s)))

      def onEffect[X](sx: Safe[X], continuation: Arrs[R, X, A], s: S): (Eff[R, A], S) Xor Eff[U, Out] = //???
        sx match {
          case EvaluateValue(v) =>
            Xor.catchNonFatal(v.value) match {
              case Xor.Left(e) =>
                Xor.Right(pure((Xor.Left(e), s)))

              case Xor.Right(x) =>
                Xor.Left((continuation(x), s))
            }

          case FailedValue(t) =>
            Xor.Right(pure((Xor.Left(t), s)))

          case FailedFinalizer(t) =>
           Xor.Left((continuation(()), s :+ t))
        }
    }

    interpretLoop1[R, U, Safe, A, Out]((a: A) => (Xor.Right(a), Vector.empty): Out)(loop)(r).map { case (a, vs) => (a, vs.toList) }
  }

  /**
   * evaluate 1 action possibly having error effects
   * execute a second action whether the first is successful or not but keep track of finalizer exceptions
   */
  def andFinally[R, A](action: Eff[R, A], last: Eff[R, Unit])(implicit m: Safe <= R): Eff[R, A] = {
    val loop = new StatelessLoop[Safe, R, A, Eff[R, A]] {
      def onPure(a: A): Eff[R, A] Xor Eff[R, A] =
        Xor.Right(pure(a))

      def onEffect[X](sx: Safe[X], continuation: Arrs[R, X, A]): Eff[R, A] Xor Eff[R, A] =
        sx match {
          case EvaluateValue(v) =>
            Xor.catchNonFatal(v.value) match {
              case Xor.Left(e) =>
                Xor.Right(attempt(last) flatMap {
                  case Xor.Left(t)   => outer.finalizerException[R](t) >> outer.exception[R, A](e)
                  case Xor.Right(()) => outer.exception[R, A](e)
                })

              case Xor.Right(x) =>
                Xor.Left(continuation(x))
            }

          case FailedValue(t) =>
            Xor.Left(outer.exception(t))

          case FailedFinalizer(t) =>
            Xor.Left(outer.finalizerException(t) >> continuation(()))
        }
    }

    interceptStatelessLoop1[R, Safe, A, A]((a: A) => a)(loop)(action)
  }

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful
   */
  def orElse[R, A](action: Eff[R, A], onThrowable: Eff[R, A])(implicit m: Safe <= R): Eff[R, A] =
    whenFailed(action, _ => onThrowable)

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful, based on the error
   */
  def catchThrowable[R, A, B](action: Eff[R, A], pure: A => B, onThrowable: Throwable => Eff[R, B])(implicit m: Safe <= R): Eff[R, B] = {
    val recurse = new Recurse[Safe, R, B] {
      def apply[X](current: Safe[X]): X Xor Eff[R, B] =
        current match {
          case EvaluateValue(v) =>
            Xor.catchNonFatal(v.value).leftMap(onThrowable).swap

          case FailedValue(t) =>
            Xor.Right(outer.exception(t))

          case FailedFinalizer(t) =>
            Xor.Right(outer.exception(t))
        }
    }
    intercept1[R, Safe, A, B](pure)(recurse)(action)
  }


  /**
   * evaluate 1 action possibly throwing exceptions
   *
   * Execute a second action if the first one is not successful, based on the exception
   *
   * The final value type is the same as the original type
   */
  def whenFailed[R, A](action: Eff[R, A], onError: Throwable => Eff[R, A])(implicit m: Safe <= R): Eff[R, A] =
    catchThrowable(action, identity[A], onError)

  /**
   * try to execute an action an report any issue
   */
  def attempt[R, A](action: Eff[R, A])(implicit m: Safe <= R): Eff[R, Throwable Xor A] =
    catchThrowable(action, Xor.right[Throwable, A], (t: Throwable) => pure(Xor.Left(t)))

  /**
   * ignore one possible exception that could be thrown
   */
  def ignoreException[R, E <: Throwable : ClassTag, A](action: Eff[R, A])(implicit m: Safe <= R): Eff[R, Unit] =
    catchThrowable[R, A, Unit](action, (a: A) => (), {
      case t if implicitly[ClassTag[E]].runtimeClass.isInstance(t) => pure(())
      case t => outer.exception(t)
    })

}
