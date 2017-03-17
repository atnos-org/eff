package org.atnos.eff

import cats._
import cats.implicits._
import Eff._
import Interpret._

/**
 * Effect for computation which can fail
 */
trait EitherEffect extends
  EitherCreation with
  EitherInterpretation

object EitherEffect extends EitherEffect

trait EitherCreation {

  type ThrowableEither[A] = Throwable Either A
  type _ThrowableEither[R] = ThrowableEither <= R
  type _throwableEither[R] = ThrowableEither |= R

  /** create an Either effect from a single Option value */
  def optionEither[R, E, A](option: Option[A], e: E)(implicit member: (E Either ?) |= R): Eff[R, A] =
    option.fold[Eff[R, A]](left[R, E, A](e))(right[R, E, A])

  /** create an Either effect from a single Either value */
  def fromEither[R, E, A](Either: E Either A)(implicit member: (E Either ?) |= R): Eff[R, A] =
    Either.fold[Eff[R, A]](left[R, E, A], right[R, E, A])

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: (E Either ?) |= R): Eff[R, A] =
    send[E Either ?, R, A](Left(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: (E Either ?) |= R): Eff[R, A] =
    send[E Either ?, R, A](Right(a))

  /** create an Either effect from a value possibly throwing an exception */
  def fromCatchNonFatal[R, E, A](a: =>A)(onThrowable: Throwable => E)(implicit member: (E Either ?) |= R): Eff[R, A] =
    fromEither(Either.catchNonFatal(a).leftMap(onThrowable))

  /** create an Either effect from a value possibly throwing a Throwable */
  def catchNonFatalThrowable[R, A](a: =>A)(implicit member: (Throwable Either ?) |= R): Eff[R, A] =
    fromCatchNonFatal(a)(identity)
}

object EitherCreation extends EitherCreation

trait EitherInterpretation {

  /** run the Either effect, yielding E Either A */
  def runEither[R, U, E, A](effect: Eff[R, A])(implicit m: Member.Aux[(E Either ?), R, U]): Eff[U, E Either A] =
    Interpret.interpretGeneric(effect)(Interpreter.fromRecurser(new Recurser[E Either ?, U, A, E Either A] {
      def onPure(a: A): Eff[U, E Either A] =
        Eff.pure(Right(a))

      def onEffect[X](m: E Either X): X Either Eff[U, E Either A] =
        m match {
          case Left(e)  => Right(EffMonad[U].pure(Left(e)))
          case Right(a) => Left(a)
        }

      def onApplicative[X, T[_]: Traverse](ms: T[E Either X]): T[X] Either (E Either T[X]) =
        ms.sequence match {
          case Left(e)       => Right(Left(e))
          case Right(values) => Left(values)
        }
    }))

  /** run the Either effect, yielding E Either A and combine all Es */
  def runEitherCombine[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E Either ?), R, U], s: Semigroup[E]): Eff[U, E Either A] = {
    val recurse = new Recurse[(E Either ?), U, E Either A] {
      def apply[X](m: E Either X) =
        m match {
          case Left(e) => Right(EffMonad[U].pure(Left(e)))
          case Right(a) => Left(a)
        }

      def applicative[X, T[_] : Traverse](ms: T[E Either X]): T[X] Either (E Either T[X]) = {
        val ap = EitherApplicative[E]
        Right(ms.sequence(implicitly[Either[E, X] <:< Either[E, X]], ap))
      }
    }

    interpret1[R, U, (E Either ?), A, E Either A]((a: A) => Right(a): E Either A)(recurse)(r)
  }

  /** catch and handle a possible left value */
  def catchLeft[R, E, A](r: Eff[R, A])(handle: E => Eff[R, A])(implicit member: (E Either ?) /= R): Eff[R, A] = {
    val recurse = new Recurse[(E Either ?), R, A] {
      def apply[X](m: E Either X) =
        m match {
          case Left(e) => Right(handle(e))
          case Right(a) => Left(a)
        }

      def applicative[X, T[_]: Traverse](ms: T[E Either X]): T[X] Either (E Either T[X]) =
        Right(ms.sequence)
    }

    intercept1[R, (E Either ?), A, A]((a: A) => a)(recurse)(r)
  }

  /** run the Either effect, handling E (with effects) and yielding A */
  def runEitherCatchLeft[R, U, E, A](r: Eff[R, A])(handle: E => Eff[U, A])(implicit m: Member.Aux[(E Either ?), R, U]): Eff[U, A] =
    runEither(r).flatMap(_.fold(handle, pure))

  /** catch and handle a possible left value. The value is the combination of all failures in case of an applicative */
  def catchLeftCombine[R, E, A](r: Eff[R, A])(handle: E => Eff[R, A])(implicit member: (E Either ?) /= R, s: Semigroup[E]): Eff[R, A] = {
    val recurse = new Recurse[(E Either ?), R, A] {
      def apply[X](m: E Either X) =
        m match {
          case Left(e) => Right(handle(e))
          case Right(a) => Left(a)
        }

      def applicative[X, T[_]: Traverse](ms: T[E Either X]): T[X] Either (E Either T[X]) = {
        val ap = EitherApplicative[E]
        Right(ms.sequence(implicitly[Either[E, X] <:< Either[E, X]], ap))
      }
    }

    intercept1[R, (E Either ?), A, A]((a: A) => a)(recurse)(r)
  }

  /**
   * Modify the type of the read value
   *
   * This changes the stack of the Eff computation
   */
  def zoomEither[SR, BR, U, E1, E2, A](r: Eff[SR, A], getter: E1 => E2)(implicit sr: Member.Aux[E1 Either ?, SR, U],
                                                                                 br: Member.Aux[E2 Either ?, BR, U]): Eff[BR, A] =
    transform[SR, BR, U, E1 Either ?, E2 Either ?, A](r,
      new ~>[E1 Either ?, E2 Either ?] {
        def apply[X](r: E1 Either X): E2 Either X =
          r.leftMap(getter)
      })

  /**
   * Translate an error effect to another one in the same stack
   * a computation over a "bigger" error (for the full application)
   */
  def translateEither[R, U, E1, E2, A](r: Eff[R, A], getter: E1 => E2)
                                      (implicit sr: Member.Aux[E1 Either ?, R, U], br: (E2 Either ?) |= U): Eff[U, A] =
    translate(r) { new Translate[E1 Either ?, U] {
      def apply[X](ex: E1 Either X): Eff[U, X] =
        ex match {
          case Left(e1) => EitherEffect.left[U, E2, X](getter(e1))
          case Right(x) => pure(x)
        }
    }}

  /**
   * Update the error value, the stack of the Eff computation stays the same
   */
  def localEither[R, E, A](e: Eff[R, A])(modify: E => E)(implicit m: (E Either ?) /= R): Eff[R, A] =
    interceptNat(e)(new ~>[E Either ?, E Either ?] {
      def apply[X](ex: E Either X): E Either X =
        ex.leftMap(modify)
    })

  def EitherApplicative[E](implicit s: Semigroup[E]): Applicative[E Either ?] = new Applicative[E Either ?] {
    def pure[A](a: A) = Right(a)

    def ap[A, B](ff: E Either (A => B))(fa: E Either A): E Either B =
      fa match {
        case Right(a) => ff.map(_(a))
        case Left(e1) => ff match {
          case Right(_) => Left(e1)
          case Left(e2) => Left(s.combine(e1, e2))
        }
      }
  }

}

trait EitherImplicits {

  implicit final def errorTranslate[R, E1, E2](implicit m: MemberIn[E1 Either ?, R], map: E2 => E1): MemberIn[E2 Either ?, R] =
    m.transform(errorTranslateNat(map))

  final def errorTranslateNat[E1, E2](map: E2 => E1): (E2 Either ?) ~> (E1 Either ?) = new ((E2 Either ?) ~> (E1 Either ?)) {
    def apply[X](x2: E2 Either X): E1 Either X = x2.leftMap(map)
  }

}

object EitherImplicits extends EitherImplicits

object EitherInterpretation extends EitherInterpretation
