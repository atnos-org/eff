package org.atnos.eff

import scala.util.control.NonFatal
import cats._
import cats.syntax.all._
import Eff._
import Interpret._
import scala.reflect.ClassTag

trait ErrorInterpretation[F] extends ErrorCreation[F] { outer =>

  /**
   * Run an error effect.
   *
   * Stop all computation if there is an exception or a failure.
   */
  def runError[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[ErrorOrOk, R, U]): Eff[U, Error Either A] =
    runInterpreter(r)(errorInterpreter[U, A, Error Either A](a => Right(a), e => Eff.pure(Left(e))))

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful, based on the error
   */
  def catchError[R, A, B](action: Eff[R, A], pure: A => B, onError: Error => Eff[R, B])(implicit m: ErrorOrOk /= R): Eff[R, B] =
    intercept(action)(errorInterpreter(pure, onError))

  def errorInterpreter[R, A, B](pureValue: A => B, onError: Error => Eff[R, B]): Interpreter[ErrorOrOk, R, A, B] =
    new Interpreter[ErrorOrOk, R, A, B] {
      def onPure(a: A): Eff[R, B] =
        Eff.pure(pureValue(a))

      def onEffect[X](ex: ErrorOrOk[X], continuation: Continuation[R, X, B]): Eff[R, B] =
        ex.run match {
          case Left(e) => onError(e)

          case Right(a) =>
            try Eff.impure(a.value, continuation)
            catch { case NonFatal(t) => onError(Left(t)) }
        }

      def onLastEffect[X](x: ErrorOrOk[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] =
        Eff.pure(())

      def onApplicativeEffect[X, T[_]: Traverse](ms: T[ErrorOrOk[X]], continuation: Continuation[R, T[X], B]): Eff[R, B] =
        ms.traverse(_.run) match {
          case Left(e) => onError(e)
          case Right(ls) =>
            try Eff.impure(ls.map(_.value), continuation)
            catch { case NonFatal(t) => onError(Left(t)) }
        }
    }

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action whether the first is successful or not
   */
  def andFinally[R, A](action: Eff[R, A], lastAction: Eff[R, Unit])(implicit m: ErrorOrOk /= R): Eff[R, A] =
    intercept(action)(new Interpreter[ErrorOrOk, R, A, A] {
      def onPure(a: A): Eff[R, A] =
        lastAction.as(a)

      def onEffect[X](ex: ErrorOrOk[X], continuation: Continuation[R, X, A]): Eff[R, A] =
        ex.run match {
          case Left(e) => continuation.runOnNone >> lastAction.flatMap(_ => outer.error[R, A](e))
          case Right(x) =>
            try Eff.impure(x.value, continuation)
            catch { case NonFatal(t) => continuation.runOnNone >> lastAction.flatMap(_ => outer.exception[R, A](t)) }
        }

      def onLastEffect[X](x: ErrorOrOk[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] =
        Eff.pure(())

      def onApplicativeEffect[X, T[_]: Traverse](ms: T[ErrorOrOk[X]], continuation: Continuation[R, T[X], A]): Eff[R, A] =
        ms.map(_.run).sequence match {
          case Left(e) => continuation.runOnNone >> lastAction.flatMap(_ => outer.error[R, A](e))
          case Right(ls) =>
            try Eff.impure(ls.map(_.value), continuation)
            catch { case NonFatal(t) => continuation.runOnNone >> lastAction.flatMap(_ => outer.exception[R, A](t)) }
        }

    })

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful
   */
  def orElse[R, A](action: Eff[R, A], onError: Eff[R, A])(implicit m: ErrorOrOk /= R): Eff[R, A] =
    whenFailed(action, _ => onError)

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful, based on the error
   *
   * The final value type is the same as the original type
   */
  def whenFailed[R, A](action: Eff[R, A], onError: Error => Eff[R, A])(implicit m: ErrorOrOk /= R): Eff[R, A] =
    catchError(action, identity[A], onError)

  /**
   * ignore one possible exception that could be thrown
   */
  def ignoreException[R, E <: Throwable: ClassTag, A](action: Eff[R, A])(implicit m: ErrorOrOk /= R): Eff[R, Unit] =
    catchError[R, A, Unit](
      action,
      (a: A) => (),
      {
        case Left(t) if implicitly[ClassTag[E]].runtimeClass.isInstance(t) =>
          EffMonad[R].pure(())
        case other => outer.error(other)
      }
    )

  /**
   * Lift a computation over a "small" error (for a subsystem) into
   * a computation over a "bigger" error (for the full application)
   */
  def localError[SR, BR, U1, U2, F1, F2, A](r: Eff[SR, A], getter: F1 => F2)(implicit
    sr: Member.Aux[Evaluate[F1, *], SR, U1],
    br: Member.Aux[Evaluate[F2, *], BR, U2],
    into: IntoPoly[U1, U2]
  ): Eff[BR, A] =
    transform[SR, BR, U1, U2, Evaluate[F1, *], Evaluate[F2, *], A](
      r,
      new ~>[Evaluate[F1, *], Evaluate[F2, *]] {
        def apply[X](r: Evaluate[F1, X]): Evaluate[F2, X] =
          Evaluate(r.run.leftMap(_.map(getter)))
      }
    )

  /**
    * Translate an error effect to another one in the same stack
    * a computation over a "bigger" error (for the full application)
    */
  def runLocalError[R, U, F1, F2, A](r: Eff[R, A], getter: F1 => F2)(implicit
    sr: Member.Aux[Evaluate[F1, *], R, U],
    br: Evaluate[F2, *] |= U
  ): Eff[U, A] =
    translate[R, U, Evaluate[F1, *], A](r) {
      new Translate[Evaluate[F1, *], U] {
        def apply[X](ex: Evaluate[F1, X]): Eff[U, X] =
          ex.run match {
            case Left(Left(t)) => send[Evaluate[F2, *], U, X](Evaluate.exception[F2, X](t))
            case Left(Right(e1)) => send[Evaluate[F2, *], U, X](Evaluate.fail[F2, X](getter(e1)))
            case Right(x) => send[Evaluate[F2, *], U, X](Evaluate.eval[F2, X](x))
          }
      }
    }

}
