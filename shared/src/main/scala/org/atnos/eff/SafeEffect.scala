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

  type _Safe[R] = Safe <= R
  type _safe[R] = Safe |= R
}

trait SafeCreation extends SafeTypes {

  def protect[R :_safe, A](a: =>A): Eff[R, A] =
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
   * Run a safe effect
   *
   * Collect finalizer exceptions if any
   */
  def runSafe[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Safe, R, U]): Eff[U, (ThrowableXor[A], List[Throwable])] = {
    type Out = (ThrowableXor[A], Vector[Throwable])
    val loop = new Loop[Safe, R, A, Eff[U, Out]] {
      type S = Vector[Throwable]
      val init: S = Vector.empty[Throwable]

      def onPure(a: A, s: S): (Eff[R, A], S) Xor Eff[U, Out] =
        Xor.Right(pure((Xor.Right(a), s)))

      def onEffect[X](sx: Safe[X], continuation: Arrs[R, X, A], s: S): (Eff[R, A], S) Xor Eff[U, Out] =
        sx match {
          case EvaluateValue(v) =>
            Xor.catchNonFatal(v.value) match {
              case Xor.Left(e) =>
                Xor.Right(pure((Xor.Left(e), s)))

              case Xor.Right(x) =>
                Xor.catchNonFatal(continuation(x)) match {
                  case Xor.Left(e) => Xor.Right(pure((Xor.Left(e), s)))
                  case Xor.Right(c) =>  Xor.Left((c, s))
                }
            }

          case FailedValue(t) =>
            Xor.Right(pure((Xor.Left(t), s)))

          case FailedFinalizer(t) =>
            Xor.catchNonFatal(continuation(())) match {
              case Xor.Left(e)  => Xor.Right(pure((Xor.Left(e), s :+ t)))
              case Xor.Right(c) => Xor.Left((c, s :+ t))
            }
        }

      def onApplicativeEffect[X](xs: List[Safe[X]], continuation: Arrs[R, List[X], A], s: S): (Eff[R, A], S) Xor Eff[U, Out] = {
        val previousFailures = xs.collect { case FailedValue(t) => t; case FailedFinalizer(t) => t }
        previousFailures match {
          case t :: rest =>
            Xor.Right(pure((Xor.Left(t), xs.collect { case FailedFinalizer(e) => e  }.toVector ++ s)))

          case Nil =>
            val executed = xs.collect { case EvaluateValue(a) => Xor.catchNonFatal(a.value).fold(t => Xor.Left[FailedValue[X]](FailedValue(t)), x => Xor.Right[X](x)) }

            val (failures, successes) = executed.separate

            failures match {
              case Nil =>
                Xor.Left((continuation(successes), s))

              case FailedValue(throwable) :: rest =>
                val issues = (previousFailures ++ rest).collect { case FailedFinalizer(t) => t }.toVector
                Xor.Right(pure((Xor.Left(throwable), issues ++ s)))
            }
        }
      }
    }

    interpretLoop1[R, U, Safe, A, Out]((a: A) => (Xor.Right(a), Vector.empty): Out)(loop)(r).map { case (a, vs) => (a, vs.toList) }
  }

  /** run a safe effect but drop the finalizer errors */
  def execSafe[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Safe, R, U]): Eff[U, ThrowableXor[A]] =
    runSafe(r).map(_._1)

  /**
   * Attempt to execute a safe action including finalizers
   */
  def attemptSafe[R, A](r: Eff[R, A])(implicit m: Safe <= R): Eff[R, (ThrowableXor[A], List[Throwable])] = {
    type Out = (ThrowableXor[A], Vector[Throwable])
    val loop = new Loop[Safe, R, A, Eff[R, Out]] {
      type S = Vector[Throwable]
      val init: S = Vector.empty[Throwable]

      def onPure(a: A, s: S): (Eff[R, A], S) Xor Eff[R, Out] =
        Xor.Right(pure((Xor.Right(a), s)))

      def onEffect[X](sx: Safe[X], continuation: Arrs[R, X, A], s: S): (Eff[R, A], S) Xor Eff[R, Out] =
        sx match {
          case EvaluateValue(v) =>
            Xor.catchNonFatal(v.value) match {
              case Xor.Left(e) =>
                Xor.Right(pure((Xor.Left(e), s)))

              case Xor.Right(x) =>
                Xor.catchNonFatal(continuation(x)) match {
                  case Xor.Left(e) => Xor.Right(pure((Xor.Left(e), s)))
                  case Xor.Right(c) =>  Xor.Left((c, s))
                }
            }

          case FailedValue(t) =>
            Xor.Right(pure((Xor.Left(t), s)))

          case FailedFinalizer(t) =>
            Xor.catchNonFatal(continuation(())) match {
              case Xor.Left(e) => Xor.Right(pure((Xor.Left(e), s :+ t)))
              case Xor.Right(c) =>  Xor.Left((c, s :+ t))
            }
        }

      def onApplicativeEffect[X](xs: List[Safe[X]], continuation: Arrs[R, List[X], A], s: S): (Eff[R, A], S) Xor Eff[R, Out] =  {
        val previousFailures = xs.filter { case EvaluateValue(t) => false; case _ => true }
        val executed = xs.collect { case EvaluateValue(a) => Xor.catchNonFatal(a.value).fold(t => Xor.Left[FailedValue[X]](FailedValue(t)), x => Xor.Right[X](x)) }

        val (failures, successes) = executed.separate

        failures match {
          case Nil =>
            Xor.Left((continuation(successes), s))

          case FailedValue(throwable) :: rest =>
            Xor.Right {
              val issues = (previousFailures ++ rest).filter { case EvaluateValue(t) => false; case _ => true }
              issues.map(f => send[Safe, R, X](f)).sequence >> outer.exception(throwable)
            }
        }
      }

    }

    interceptLoop1[R, Safe, A, Out]((a: A) => (Xor.Right(a), Vector.empty): Out)(loop)(r).map { case (a, vs) => (a, vs.toList) }
  }

  /**
   * evaluate 1 action possibly having error effects
   * execute a second action whether the first is successful or not but keep track of finalizer exceptions
   */
  def andFinally[R, A](action: Eff[R, A], last: Eff[R, Unit])(implicit m: Safe <= R): Eff[R, A] = {
    val loop = new StatelessLoop[Safe, R, A, Eff[R, A]] {
      def onPure(a: A): Eff[R, A] Xor Eff[R, A] =
        Xor.Right(attempt(last) flatMap {
          case Xor.Left(t)   => outer.finalizerException[R](t) >> pure(a)
          case Xor.Right(()) => pure(a)
        })

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
                Xor.Left(attempt(last) flatMap {
                  case Xor.Left(t)   => outer.finalizerException[R](t) >> continuation(x)
                  case Xor.Right(()) => continuation(x)
                })
            }

          case FailedValue(t) =>
            Xor.Right(outer.exception(t))

          case FailedFinalizer(t) =>
            Xor.Right(outer.finalizerException(t) >> continuation(()))
        }

      def onApplicativeEffect[X](xs: List[Safe[X]], continuation: Arrs[R, List[X], A]): Eff[R, A] Xor Eff[R, A] = {
        val previousFailures = xs.filter { case EvaluateValue(t) => false; case _ => true }

        val executed = xs.collect { case EvaluateValue(a) =>
          Xor.catchNonFatal(a.value).fold(t => Xor.Left[FailedValue[X]](FailedValue(t)), x => Xor.Right[X](x))
        }

        val (failures, successes) = executed.separate

        failures match {
          case Nil =>
            Xor.Left(previousFailures.map(f => send[Safe, R, X](f)).sequence *> continuation(successes))

          case FailedValue(throwable) :: rest =>
            Xor.Left {
              val issues =
                (previousFailures ++ rest).filter { case EvaluateValue(t) => false; case _ => true }.
                  map(f => send[Safe, R, X](f)).sequence

              attempt(last) flatMap {
                case Xor.Left(t)   => outer.finalizerException[R](t) >> outer.exception[R, A](throwable)
                case Xor.Right(()) => issues >> exception[R, A](throwable)
              }
            }
        }
      }

    }

    interceptStatelessLoop1[R, Safe, A, A]((a: A) => a)(loop)(action)
  }

  def bracket[R, A, B, C](acquire: Eff[R, A])(step: A => Eff[R, B])(release: A => Eff[R, C])(implicit m: Safe <= R): Eff[R, B] =
    for {
      a <- acquire
      b <- andFinally(step(a), release(a).void)
    } yield b

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful
   */
  def otherwise[R, A](action: Eff[R, A], onThrowable: Eff[R, A])(implicit m: Safe <= R): Eff[R, A] =
    whenFailed(action, _ => onThrowable)

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful, based on the error
   */
  def catchThrowable[R, A, B](action: Eff[R, A], pureValue: A => B, onThrowable: Throwable => Eff[R, B])(implicit m: Safe <= R): Eff[R, B] =
    attemptSafe(action).flatMap {
      case (Xor.Left(t), ls)  => onThrowable(t).flatMap(b => ls.traverse(f => finalizerException(f)).as(b))
      case (Xor.Right(a), ls) => pure(pureValue(a)).flatMap(b => ls.traverse(f => finalizerException(f)).as(b))
    }

  /**
   * evaluate 1 action possibly throwing exceptions
   *
   * Execute a second action if the first one is not successful, based on the exception
   *
   * The final value type is the same as the original type
   */
  def whenFailed[R, A](action: Eff[R, A], onThrowable: Throwable => Eff[R, A])(implicit m: Safe <= R): Eff[R, A] =
    catchThrowable(action, identity[A], onThrowable)

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

object SafeInterpretation extends SafeInterpretation

/**
 * The Safe type is a mix of a ThrowableXor / Eval effect
 *   and a writer effect to collect finalizer failures
 */
sealed trait Safe[A]

case class EvaluateValue[A](a: Eval[A])  extends Safe[A]
case class FailedValue[A](t: Throwable)  extends Safe[A]
case class FailedFinalizer(t: Throwable) extends Safe[Unit]
