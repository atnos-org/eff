package org.atnos.eff

import cats.data._, Xor._
import Interpret._
import Eff._

/**
 * Effect for optional computations
 */
object OptionEffect {

  /** create an Option effect from a single Option value */
  def fromOption[R, A](option: Option[A])(implicit member: Member[Option, R]): Eff[R, A] =
    option.fold[Eff[R, A]](none)(some)

  /** no value returned */
  def none[R, A](implicit member: Member[Option, R]): Eff[R, A] =
    send[Option, R, A](None)

  /** a value is returned */
  def some[R, A](a: A)(implicit member: Member[Option, R]): Eff[R, A] =
    send[Option, R, A](Some(a))

  /**
   * Interpret the Option effect
   *
   * Stop all computations if None is present once
   */
  def runOption[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member.Aux[Option, R, U]): Eff[U, Option[A]] = {
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

