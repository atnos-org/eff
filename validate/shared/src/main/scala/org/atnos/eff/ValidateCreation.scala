package org.atnos.eff

import cats.*
import cats.data.*

trait ValidateCreation {

  /** create an Validate effect from a single Option value */
  def validateOption[R, E, A](option: Option[A], e: => E)(implicit m: Validate[E, *] |= R): Eff[R, Unit] =
    option.map(_ => correct(())).getOrElse(wrong(e))

  /** create an Validate effect from a single Either value */
  def validateEither[R, E, A](either: Either[E, A])(implicit m: Validate[E, *] |= R): Eff[R, Unit] =
    either.fold(e => wrong(e), _ => correct(()))

  /** create an Validate effect from a single Ior value */
  def validateIor[R, E, A](ior: Ior[E, A])(implicit m: Validate[E, *] |= R): Eff[R, Unit] =
    ior.fold(e => wrong(e), _ => correct(()), (w, _) => warning(w))

  /** create a failed value */
  def wrong[R, E](e: E)(implicit m: Validate[E, *] |= R): Eff[R, Unit] =
    Eff.send[Validate[E, *], R, Unit](Wrong(e))

  /** create a correct value */
  def correct[R, E, A](a: A)(implicit m: Validate[E, *] |= R): Eff[R, A] =
    Eff.send[Validate[E, *], R, Unit](Correct[E]()) >> Monad[Eff[R, *]].pure(a)

  /** create a pure warning */
  def warning[R, E](e: E)(implicit m: Validate[E, *] |= R): Eff[R, Unit] =
    Eff.send[Validate[E, *], R, Unit](Warning[E](e))

  /** create a correct value with warning */
  def warning[R, E, A](a: A, e: E)(implicit m: Validate[E, *] |= R): Eff[R, A] =
    warning(e) >> Monad[Eff[R, *]].pure(a)

  /** check a correct condition */
  def validateCheck[R, E](condition: Boolean, e: => E)(implicit m: Validate[E, *] |= R): Eff[R, Unit] =
    if (condition) correct(()) else wrong(e)

  /** check a correct value */
  def validateValue[R, E, A](condition: Boolean, a: => A, e: => E)(implicit m: Validate[E, *] |= R): Eff[R, A] =
    if (condition) correct(a) else wrong(e) >> Monad[Eff[R, *]].pure(a)
}

object ValidateCreation extends ValidateCreation
