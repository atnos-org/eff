package org.atnos.eff

import cats.implicits._
import Interpret._
import Eff._
import cats._

/**
 * Effect for optional computations
 */
trait OptionEffect extends
  OptionCreation with
  OptionInterpretation

object OptionEffect extends OptionEffect

trait OptionCreation {
  type _Option[R] = Option <= R
  type _option[R] = Option |= R

  /** create an Option effect from a single Option value */
  def fromOption[R :_option, A](o: Option[A]): Eff[R, A] =
    send[Option, R, A](o)

  /** no value returned */
  def none[R :_option, A]: Eff[R, A] =
    send[Option, R, A](None)

  /** a value is returned */
  def some[R :_option, A](a: A): Eff[R, A] =
    send[Option, R, A](Some(a))
}

object OptionCreation extends OptionCreation

trait OptionInterpretation {
  /**
   * Interpret the Option effect
   *
   * Stop all computations if None is present once
   */
  def runOption[R, U, A](effect: Eff[R, A])(implicit m: Member.Aux[Option, R, U]): Eff[U, Option[A]] =
    recurse(effect)(new Recurser[Option, U, A, Option[A]] {
      def onPure(a: A): Option[A] =
        Option(a)

      def onEffect[X](m: Option[X]): X Either Eff[U, Option[A]] =
        m match {
          case None    => Right(Eff.pure(None))
          case Some(x) => Left(x)
         }

      def onApplicative[X, T[_]: Traverse](ms: T[Option[X]]): T[X] Either Option[T[X]] =
        Right(ms.sequence)
    })

}

object OptionInterpretation extends OptionInterpretation

