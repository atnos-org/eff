package org.atnos.eff

import cats.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import org.atnos.eff.Eff.pure
import org.atnos.eff.Interpret.*

trait EitherInterpretation {

  /** run the Either effect, yielding E Either A */
  def runEither[R, U, E, A](effect: Eff[R, A])(implicit m: Member.Aux[Either[E, *], R, U]): Eff[U, Either[E, A]] =
    interpretEither(effect)(cats.instances.either.catsStdInstancesForEither[E])

  /** run the Either effect, yielding E Either A and combine all Es */
  def runEitherCombine[R, U, E, A](effect: Eff[R, A])(implicit m: Member.Aux[Either[E, *], R, U], s: Semigroup[E]): Eff[U, Either[E, A]] =
    interpretEither(effect)(EitherApplicative[E])

  private def interpretEither[R, U, E, A](effect: Eff[R, A])(ap: Applicative[Either[E, *]])(implicit
    m: Member.Aux[Either[E, *], R, U]
  ): Eff[U, Either[E, A]] =
    Interpret.recurse(effect)(eitherRecurser[U, E, A, Either[E, A]](a => Right(a), e => Monad[Eff[U, *]].pure(Left(e)))(ap))

  /** catch possible left values */
  def attemptEither[R, E, A](effect: Eff[R, A])(implicit member: Either[E, *] /= R): Eff[R, Either[E, A]] =
    catchLeft[R, E, Either[E, A]](effect.map(a => Either.right(a)))(e => pure(Either.left(e)))

  /** catch and handle a possible left value */
  def catchLeft[R, E, A](effect: Eff[R, A])(handle: E => Eff[R, A])(implicit member: Either[E, *] /= R): Eff[R, A] =
    catchLeftEither[R, E, A](effect)(handle)(cats.instances.either.catsStdInstancesForEither[E])

  /** run the Either effect, handling E (with effects) and yielding A */
  def runEitherCatchLeft[R, U, E, A](r: Eff[R, A])(handle: E => Eff[U, A])(implicit m: Member.Aux[Either[E, *], R, U]): Eff[U, A] =
    runEither(r).flatMap(_.fold(handle, pure))

  /** catch and handle a possible left value. The value is the combination of all failures in case of an applicative */
  def catchLeftCombine[R, E, A](effect: Eff[R, A])(handle: E => Eff[R, A])(implicit member: Either[E, *] /= R, s: Semigroup[E]): Eff[R, A] =
    catchLeftEither[R, E, A](effect)(handle)(EitherApplicative[E])

  private def catchLeftEither[R, E, A](effect: Eff[R, A])(handle: E => Eff[R, A])(ap: Applicative[Either[E, *]])(implicit
    member: Either[E, *] /= R
  ): Eff[R, A] =
    Interpret.intercept(effect)(Interpreter.fromRecurser(eitherRecurser[R, E, A, A](a => a, handle)(ap)))

  private def eitherRecurser[R, E, A, B](pureValue: A => B, handle: E => Eff[R, B])(ap: Applicative[Either[E, *]]): Recurser[Either[E, *], R, A, B] =
    new Recurser[Either[E, *], R, A, B] {
      def onPure(a: A): B =
        pureValue(a)

      def onEffect[X](m: Either[E, X]): Either[X, Eff[R, B]] =
        m match {
          case Left(e) => Right(handle(e))
          case Right(a) => Left(a)
        }

      def onApplicative[X, T[_]: Traverse](ms: T[Either[E, X]]): Either[T[X], Either[E, T[X]]] = {
        implicit val eitherAp: Applicative[Either[E, *]] = ap
        Right(ms.sequence)
      }
    }

  /**
   * Modify the type of the read value
   *
   * This changes the stack of the Eff computation
   */
  def zoomEither[SR, BR, U1, U2, E1, E2, A](r: Eff[SR, A], getter: E1 => E2)(implicit
    sr: Member.Aux[Either[E1, *], SR, U1],
    br: Member.Aux[Either[E2, *], BR, U2],
    into: IntoPoly[U1, U2]
  ): Eff[BR, A] =
    transform[SR, BR, U1, U2, Either[E1, *], Either[E2, *], A](
      r,
      new ~>[Either[E1, *], Either[E2, *]] {
        def apply[X](r: Either[E1, X]): Either[E2, X] =
          r.leftMap(getter)
      }
    )

  /**
   * Translate an error effect to another one in the same stack
   * a computation over a "bigger" error (for the full application)
   */
  def translateEither[R, U, E1, E2, A](r: Eff[R, A], getter: E1 => E2)(implicit
    sr: Member.Aux[Either[E1, *], R, U],
    br: Either[E2, *] |= U
  ): Eff[U, A] =
    translate(r) {
      new Translate[Either[E1, *], U] {
        def apply[X](ex: Either[E1, X]): Eff[U, X] =
          ex match {
            case Left(e1) => EitherEffect.left[U, E2, X](getter(e1))
            case Right(x) => pure(x)
          }
      }
    }

  /**
   * Update the error value, the stack of the Eff computation stays the same
   */
  def localEither[R, E, A](e: Eff[R, A])(modify: E => E)(implicit m: Either[E, *] /= R): Eff[R, A] =
    interceptNat(e)(new ~>[Either[E, *], Either[E, *]] {
      def apply[X](ex: Either[E, X]): Either[E, X] =
        ex.leftMap(modify)
    })

  def EitherApplicative[E](implicit s: Semigroup[E]): Applicative[Either[E, *]] = new Applicative[Either[E, *]] {
    def pure[A](a: A): Either[E, A] = Right(a)

    def ap[A, B](ff: Either[E, A => B])(fa: Either[E, A]): Either[E, B] =
      fa match {
        case Right(a) => ff.map(_(a))
        case Left(e1) =>
          ff match {
            case Right(_) => Left(e1)
            case Left(e2) => Left(s.combine(e1, e2))
          }
      }
  }

}

object EitherInterpretation extends EitherInterpretation
