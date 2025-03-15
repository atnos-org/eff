package org.atnos.eff

import cats.syntax.either.*
import org.atnos.eff.Eff.send

trait EitherCreation {

  type ThrowableEither[A] = Either[Throwable, A]
  type _ThrowableEither[R] = ThrowableEither <= R
  type _throwableEither[R] = ThrowableEither |= R

  /** create an Either effect from a single Option value */
  def optionEither[R, E, A](option: Option[A], e: => E)(implicit member: Either[E, *] |= R): Eff[R, A] =
    option.fold[Eff[R, A]](left[R, E, A](e))(right[R, E, A])

  /** create an Either effect from a single Either value */
  def fromEither[R, E, A](Either: Either[E, A])(implicit member: Either[E, *] |= R): Eff[R, A] =
    send[Either[E, *], R, A](Either)

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: Either[E, *] |= R): Eff[R, A] =
    send[Either[E, *], R, A](Left(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: Either[E, *] |= R): Eff[R, A] =
    send[Either[E, *], R, A](Right(a))

  /** create an Either effect from a value possibly throwing an exception */
  def fromCatchNonFatal[R, E, A](a: => A)(onThrowable: Throwable => E)(implicit member: Either[E, *] |= R): Eff[R, A] =
    fromEither(Either.catchNonFatal(a).leftMap(onThrowable))

  /** create an Either effect from a value possibly throwing a Throwable */
  def catchNonFatalThrowable[R, A](a: => A)(implicit member: ThrowableEither |= R): Eff[R, A] =
    fromCatchNonFatal(a)(identity)
}

object EitherCreation extends EitherCreation
