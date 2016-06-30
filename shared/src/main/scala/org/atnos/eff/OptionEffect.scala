package org.atnos.eff

import cats.data._, Xor._
import Interpret._
import Eff._

/**
 * Effect for optional computations
 */
trait OptionEffect extends
  OptionCreation with
  OptionInterpretation

object OptionEffect extends OptionEffect

trait OptionCreation {
  type _Option[R] = Option <= R

  /** create an Option effect from a single Option value */
  def fromOption[R :_Option, A](o: Option[A]): Eff[R, A] =
    send[Option, R, A](o)

  /** no value returned */
  def none[R :_Option, A]: Eff[R, A] =
    send[Option, R, A](None)

  /** a value is returned */
  def some[R :_Option, A](a: A): Eff[R, A] =
    send[Option, R, A](Some(a))
}

object OptionCreation extends OptionCreation

trait OptionInterpretation {
  /**
   * Interpret the Option effect
   *
   * Stop all computations if None is present once
   */
  def runOption[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Option, R, U]): Eff[U, Option[A]] = {
    val recurse = new Recurse[Option, U, Option[A]] {
      def apply[X](m: Option[X]) =
        m match {
          case None    => Right(EffMonad[U].pure(None))
          case Some(x) => Left(x)
        }
    }

    interpret1[R, U, Option, A, Option[A]]((a: A) => Option(a))(recurse)(r)
  }

}

object OptionInterpretation extends OptionInterpretation

