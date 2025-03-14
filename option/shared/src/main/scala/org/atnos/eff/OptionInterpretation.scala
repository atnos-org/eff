package org.atnos.eff

import cats.*
import cats.syntax.all.*
import org.atnos.eff.Interpret.*

trait OptionInterpretation {

  /**
   * Interpret the Option effect
   *
   * Stop all computations if None is present once
   */
  def runOption[R, U, A](effect: Eff[R, A])(implicit m: Member.Aux[Option, R, U]): Eff[U, Option[A]] =
    recurse(effect)(new Recurser[Option, U, A, Option[A]] {
      def onPure(a: A): Option[A] =
        Some(a)

      def onEffect[X](m: Option[X]): Either[X, Eff[U, Option[A]]] =
        m match {
          case None => Right(Eff.pure(None))
          case Some(x) => Left(x)
        }

      def onApplicative[X, T[_]: Traverse](ms: T[Option[X]]): Either[T[X], Option[T[X]]] =
        Right(ms.sequence)
    })

}

object OptionInterpretation extends OptionInterpretation
