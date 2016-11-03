package org.atnos.eff

import cats._, data._
import cats.implicits._
import org.atnos.eff.all._
import Interpret._

/**
 * Effect for computation which can fail but will accumulate errors
 *
 * The runValidate interpreter just collects the messages and returns them at the end
 *
 */
trait ValidateEffect extends
  ValidateCreation with
  ValidateInterpretation

object ValidateEffect extends ValidateEffect

sealed trait Validate[+E, A]
case class Correct[E]() extends Validate[E, Unit]
case class Wrong[E](e: E) extends Validate[E, Unit]

trait ValidateCreation {

  /** create an Validate effect from a single Option value */
  def validateOption[R, E, A](option: Option[A], e: E)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    option.map(_ => correct(())).getOrElse(wrong(e))

  /** create an Validate effect from a single Either value */
  def validateEither[R, E, A](either: E Either A)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    either.fold(e => wrong(e), _ => correct(()))

  /** create a failed value */
  def wrong[R, E](e: E)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    send[Validate[E, ?], R, Unit](Wrong(e))

  /** create a correct value */
  def correct[R, E, A](a: A)(implicit m: Validate[E, ?] |= R): Eff[R, A] =
    send[Validate[E, ?], R, Unit](Correct[E]()) >> Eff.EffMonad[R].pure(a)

  /** check a correct condition */
  def validateCheck[R, E](condition: Boolean, e: E)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    if (condition) correct(()) else wrong(e)

  /** check a correct value */
  def validateValue[R, E, A](condition: Boolean, a: A, e: E)(implicit m: Validate[E, ?] |= R): Eff[R, A] =
    if (condition) correct(a) else wrong(e) >> Eff.EffMonad[R].pure(a)
}

object ValidateCreation extends ValidateCreation

trait ValidateInterpretation extends ValidateCreation {

  /** run the validate effect, yielding a ValidatedNel */
  def runValidatedNel[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, ValidatedNel[E, A]] =
    runNel(r).map(result => Validated.fromEither(result))

  /** run the validate effect, yielding a non-empty list of failures Either A */
  def runNel[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, NonEmptyList[E] Either A] =
    runMap[R, U, E, NonEmptyList[E], A](r)((e: E) => NonEmptyList.of(e))

  /** run the validate effect, yielding a list of failures Either A */
  def runMap[R, U, E, L : Semigroup, A](r: Eff[R, A])(map: E => L)(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, L Either A] = {
    val recurse: StateRecurse[Validate[E, ?], A, L Either A] = new StateRecurse[Validate[E, ?], A, L Either A] {
      type S = Option[L]
      val init: Option[L] = None

      def apply[X](x: Validate[E, X], s: Option[L]): (X, Option[L]) =
        x match {
          case Wrong(e) => ((), s.fold(Option(map(e)))(l => Option(l |+| map(e))))
          case Correct() => ((), s)
        }

      def applicative[X, T[_] : Traverse](xs: T[Validate[E, X]], s: S): (T[X], S) Either (Validate[E, T[X]], S) =
        Left {
          val traversed: State[S, T[X]] = xs.traverse {
            case Correct() => State[S, X](state => (state, ()))
            case Wrong(e)  => State[S, X](state => (state.fold(Option(map(e)))(l => Option(l |+| map(e))), ()))
          }
          traversed.run(s).value.swap
        }

      def finalize(a: A, s: S): L Either A =
        s.fold[Either[L, A]](Right[L, A](a))(Left[L, A])
    }

    interpretState1[R, U, Validate[E, ?], A, L Either A]((a: A) => Right[L, A](a))(recurse)(r)
  }

  /** catch and handle possible wrong values */
  def catchWrong[R, E, A](r: Eff[R, A])(handle: E => Eff[R, A])(implicit member: (Validate[E, ?]) <= R): Eff[R, A] = {
    val loop = new StatelessLoop[Validate[E,?], R, A, Eff[R, A], Eff[R, Unit]] {
      def onPure(a: A): Eff[R, A] Either Eff[R, A] =
        Right(pure(a))

      def onEffect[X](m: Validate[E, X], continuation: Arrs[R, X, A]): Eff[R, A] Either Eff[R, A] =
        m match {
          case Correct() => Left(continuation(()))
          case Wrong(e)  => Left(handle(e))
        }

      def onLastEffect[X](m: Validate[E, X], continuation: Arrs[R, X, Unit]): Eff[R, Unit] Either Eff[R, Unit] =
        m match {
          case Correct() => Left(continuation(()))
          case Wrong(e)  => Left(handle(e).void)
        }

      def onApplicativeEffect[X, T[_] : Traverse](xs: T[Validate[E, X]], continuation: Arrs[R, T[X], A]): Eff[R, A] Either Eff[R, A] =
        Left {
          val traversed: State[Option[E], T[X]] = xs.traverse {
            case Correct() => State[Option[E], X](state => (None, ()))
            case Wrong(e)  => State[Option[E], X](state => (Some(e), ()))
          }

          traversed.run(None).value match {
            case (None, tx)    => continuation(tx)
            case (Some(e), tx) => handle(e)
          }
        }

      def onLastApplicativeEffect[X, T[_] : Traverse](xs: T[Validate[E, X]], continuation: Arrs[R, T[X], Unit]): Eff[R, Unit] Either Eff[R, Unit] =
        Left {
          val traversed: State[Option[E], T[X]] = xs.traverse {
            case Correct() => State[Option[E], X](state => (None, ()))
            case Wrong(e)  => State[Option[E], X](state => (Some(e), ()))
          }

          traversed.run(None).value match {
            case (None, tx)    => continuation(tx)
            case (Some(e), tx) => handle(e).void
          }
        }

    }

    interceptStatelessLoop[R, Validate[E,?], A, A]((a: A) => pure(a), loop)(r)
  }
}

object ValidateInterpretation extends ValidateInterpretation

