package org.atnos.eff

import cats._
import cats.syntax.all._
import Eff._

/**
 * Interpret eff values
 *
 * For stack-safety reasons, the continuation must *never* be called
 * with a value directly, but always with Eff.impure:
 *
 * Eff.impure(a, continuation)
 *
 * * *Note* it is the responsibility of the implementation to call continuation.onNone if
 * the continuation is not used to create the return value.
 */
trait Interpreter[M[_], R, A, B] {

  /**
   * Interpret a pure value
   */
  def onPure(a: A): Eff[R, B]

  /**
   * Interpret an effect of type M
   *
   * if the value X can be extracted call the continuation to get the next Eff[R, B] value
   * otherwise provide a Eff[R, B] value
   */
  def onEffect[X](x: M[X], continuation: Continuation[R, X, B]): Eff[R, B]

  /**
   * Interpret a side-effect of type M
   *
   * if the value X can be extracted call the continuation to get the next Eff[R, B] value
   * otherwise provide a Eff[R, B] value
   */
  def onLastEffect[X](x: M[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit]

  /**
   * Interpret a list of effects of type M
   *
   * if the value X can be extracted call the continuation to get the next Eff[R, B] value
   * otherwise provide a Eff[R, B] value
   */
  def onApplicativeEffect[X, T[_]: Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], B]): Eff[R, B]
}

object Interpreter {

  def fromRecurser[M[_], R, A, B](recurser: Recurser[M, R, A, B]): Interpreter[M, R, A, B] =
    new Interpreter[M, R, A, B] {
      def onPure(a: A): Eff[R, B] =
        Eff.pure(recurser.onPure(a))

      def onLastEffect[X](x: M[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] =
        Eff.pure(())

      def onEffect[X](mx: M[X], continuation: Continuation[R, X, B]): Eff[R, B] =
        recurser.onEffect(mx) match {
          case Left(x) => Eff.impure(x, continuation)
          case Right(b) => continuation.runOnNone >> b
        }

      def onApplicativeEffect[X, T[_]: Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], B]): Eff[R, B] =
        recurser.onApplicative(xs) match {
          case Left(x) => Eff.impure(x, continuation)
          case Right(mx) => onEffect(mx, continuation)
        }
    }

  def fromTranslate[M[_], R, A](translate: Translate[M, R]): Interpreter[M, R, A, A] =
    new Interpreter[M, R, A, A] {
      def onPure(a: A): Eff[R, A] =
        Eff.pure(a)

      def onEffect[X](x: M[X], continuation: Continuation[R, X, A]): Eff[R, A] =
        whenStopped(translate(x).flatMap(continuation), continuation.onNone)

      def onLastEffect[X](x: M[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] =
        whenStopped(translate(x).flatMap(continuation), continuation.onNone)

      def onApplicativeEffect[X, T[_]: Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], A]): Eff[R, A] =
        whenStopped(Eff.traverseA(xs)(translate.apply).flatMap(continuation), continuation.onNone)
    }

  def fromNat[M[_], N[_], R, A](nat: M ~> N)(implicit n: N |= R): Interpreter[M, R, A, A] =
    fromTranslate(new Translate[M, R] {
      def apply[X](x: M[X]): Eff[R, X] =
        Eff.send[N, R, X](nat(x))
    })

  def fromSideEffect[M[_], R, A](sideEffect: SideEffect[M]): Interpreter[M, R, A, A] =
    fromRecurser[M, R, A, A](new Recurser[M, R, A, A] {
      def onPure(a: A): A =
        a

      def onEffect[X](mx: M[X]): X Either Eff[R, A] =
        Left(sideEffect(mx))

      def onApplicative[X, T[_]: Traverse](ms: T[M[X]]): T[X] Either M[T[X]] =
        Left(ms.map(sideEffect.apply))
    })
}
