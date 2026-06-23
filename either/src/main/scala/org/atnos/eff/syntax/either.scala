package org.atnos.eff.syntax

import cats.Semigroup
import org.atnos.eff.*

object either extends either

trait either {
  given eitherExtension: AnyRef with {

    extension [R, A](e: Eff[R, A]) {

      def runEither[E](using m: Member[Either[E, *], R]): Eff[m.Out, Either[E, A]] =
        EitherInterpretation.runEither[R, m.Out, E, A](e)(using m)

      def runEitherU[E, U](using m: Member.Aux[Either[E, *], R, U]): Eff[U, Either[E, A]] =
        EitherInterpretation.runEither(e)(using m)

      def runEitherCombine[E, U](using m: Member.Aux[Either[E, *], R, U], s: Semigroup[E]): Eff[U, Either[E, A]] =
        EitherInterpretation.runEitherCombine(e)(using m, s)

      def attemptEither[E](using m: Either[E, *] /= R): Eff[R, Either[E, A]] =
        EitherInterpretation.attemptEither(e)(using m)

      def catchLeft[E](handle: E => Eff[R, A])(using member: Either[E, *] /= R): Eff[R, A] =
        EitherInterpretation.catchLeft(e)(handle)(using member)

      def runEitherCatchLeft[E, U](handle: E => Eff[U, A])(using member: Member.Aux[Either[E, *], R, U]): Eff[U, A] =
        EitherInterpretation.runEitherCatchLeft(e)(handle)(using member)

      def catchLeftCombine[E](handle: E => Eff[R, A])(using member: Either[E, *] /= R, s: Semigroup[E]): Eff[R, A] =
        EitherInterpretation.catchLeftCombine(e)(handle)(using member, s)

      def zoomEither[BR, U1, U2, C, B](
        getter: C => B
      )(using Member.Aux[Either[C, *], R, U1], Member.Aux[Either[B, *], BR, U2], IntoPoly[U1, U2]): Eff[BR, A] =
        EitherInterpretation.zoomEither[R, BR, U1, U2, C, B, A](e, getter)

      def translateEither[U, C, B](getter: C => B)(using Member.Aux[Either[C, *], R, U], Either[B, *] |= U): Eff[U, A] =
        EitherInterpretation.translateEither[R, U, C, B, A](e, getter)

      def localEither[E](modify: E => E)(using Either[E, *] /= R): Eff[R, A] =
        EitherInterpretation.localEither[R, E, A](e)(modify)
    }
  }
}
