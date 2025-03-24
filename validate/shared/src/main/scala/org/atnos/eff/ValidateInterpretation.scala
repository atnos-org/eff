package org.atnos.eff

import cats.*
import cats.data.*
import cats.syntax.all.*
import org.atnos.eff.Interpret.*

trait ValidateInterpretation extends ValidateCreation {

  /** run the validate effect, yielding a ValidatedNel */
  def runValidatedNel[R, U, E, A](r: Eff[R, A])(using Member.Aux[Validate[E, *], R, U]): Eff[U, ValidatedNel[E, A]] =
    runNel(r).map(result => Validated.fromEither(result))

  /** run the validate effect, yielding a non-empty list of failures Either A */
  def runNel[R, U, E, A](r: Eff[R, A])(using Member.Aux[Validate[E, *], R, U]): Eff[U, Either[NonEmptyList[E], A]] =
    runMap[R, U, E, NonEmptyList[E], A](r)((e: E) => NonEmptyList.of(e))

  /** run the validate effect, yielding a list of failures Either A */
  def runMap[R, U, E, L: Semigroup, A](effect: Eff[R, A])(map: E => L)(using Member.Aux[Validate[E, *], R, U]): Eff[U, Either[L, A]] =
    runMapGen(effect)(map) { (a, l) => l.map(_ => a) }

  /** run the validate effect, yielding a non-empty list of failures or A or both */
  def runIorNel[R, U, E, A](r: Eff[R, A])(using Member.Aux[Validate[E, *], R, U]): Eff[U, IorNel[E, A]] =
    runIorMap[R, U, E, NonEmptyList[E], A](r)((e: E) => NonEmptyList.one(e))

  /** run the validate effect, yielding a list of failures or A or both */
  def runIorMap[R, U, E, L: Semigroup, A](effect: Eff[R, A])(map: E => L)(using Member.Aux[Validate[E, *], R, U]): Eff[U, Ior[L, A]] =
    runMapGen(effect)(map) { (a, l) =>
      l match {
        case Left(errs) => Ior.Left(errs)
        case Right(None) => Ior.Right(a)
        case Right(Some(warns)) => Ior.Both(warns, a)
      }
    }

  private def runMapGen[R, U, E, L: Semigroup, A, SomeOr[_, _]](
    effect: Eff[R, A]
  )(map: E => L)(pure: (A, Either[L, Option[L]]) => SomeOr[L, A])(using Member.Aux[Validate[E, *], R, U]): Eff[U, SomeOr[L, A]] =
    runInterpreter(effect)(new Interpreter[Validate[E, *], U, A, SomeOr[L, A]] {
      // Left means failed, Right means not failed (Option contains warnings)
      private var l: Either[L, Option[L]] = Right(None)

      def onPure(a: A): Eff[U, SomeOr[L, A]] =
        Eff.pure(pure(a, l))

      private def combineLV[X](l: Either[L, Option[L]], v: Validate[E, X]): Either[L, Option[L]] = v match {
        case Correct() => l
        case Warning(w) =>
          l match {
            case Left(errs) => Left(errs |+| map(w))
            case Right(None) => Right(Some(map(w)))
            case Right(Some(warns)) => Right(Some(warns |+| map(w)))
          }
        case Wrong(e) =>
          l match {
            case Left(errs) => Left(errs |+| map(e))
            case Right(None) => Left(map(e))
            case Right(Some(warns)) => Left(warns |+| map(e)) // uniting warnings and errors as cats do
          }
      }

      def onEffect[X](v: Validate[E, X], continuation: Continuation[U, X, SomeOr[L, A]]): Eff[U, SomeOr[L, A]] = {
        l = combineLV(l, v)
        Eff.impure(().asInstanceOf[X], continuation)
      }

      def onLastEffect[X](x: Validate[E, X], continuation: Continuation[U, X, Unit]): Eff[U, Unit] =
        Eff.pure(())

      def onApplicativeEffect[X, T[_]: Traverse](xs: T[Validate[E, X]], continuation: Continuation[U, T[X], SomeOr[L, A]]): Eff[U, SomeOr[L, A]] = {
        l = xs.foldLeft(l)(combineLV)

        val tx: T[X] = xs.map { case Correct() | Warning(_) | Wrong(_) => () }
        Eff.impure(tx, continuation)
      }
    })

  /** catch and handle possible wrong values */
  def catchWrongs[R, E, A, S[_]: Applicative](
    effect: Eff[R, A]
  )(handle: S[E] => Eff[R, A])(using Validate[E, *] <= R, Semigroup[S[E]]): Eff[R, A] =
    intercept(effect)(new Interpreter[Validate[E, *], R, A, A] {
      private var errs: Option[S[E]] = None

      def onPure(a: A): Eff[R, A] =
        errs.map(handle).getOrElse(Eff.pure(a))

      def onEffect[X](m: Validate[E, X], continuation: Continuation[R, X, A]): Eff[R, A] = {
        val x: X = m match {
          case Correct() | Warning(_) => ()
          case Wrong(e) => {
            errs = errs |+| Some(Applicative[S].pure(e))
            ()
          }
        }
        Eff.impure(x, continuation)
      }

      def onLastEffect[X](x: Validate[E, X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] =
        continuation.runOnNone >> Eff.pure(())

      def onApplicativeEffect[X, T[_]: Traverse](xs: T[Validate[E, X]], continuation: Continuation[R, T[X], A]): Eff[R, A] = {
        val (eo, tx): (Option[S[E]], T[X]) = xs.traverse {
          case Correct() | Warning(_) => (Option.empty[S[E]], ())
          case Wrong(e) => (Some(Applicative[S].pure(e)), ())
        }

        errs = errs |+| eo
        Eff.impure(tx, continuation)
      }
    })

  /** catch and handle the first wrong value */
  def catchFirstWrong[R, E, A](effect: Eff[R, A])(handle: E => Eff[R, A])(using Validate[E, *] <= R): Eff[R, A] = {
    given Semigroup[E] = Semigroup.instance { (a, _) => a }
    catchWrongs[R, E, A, Id](effect)(handle)
  }

  /** catch and handle the last wrong value */
  def catchLastWrong[R, E, A](effect: Eff[R, A])(handle: E => Eff[R, A])(using Validate[E, *] <= R): Eff[R, A] = {
    given Semigroup[E] = Semigroup.instance { (_, b) => b }
    catchWrongs[R, E, A, Id](effect)(handle)
  }

  /** catch and handle all wrong values */
  def catchAllWrongs[R, E, A](effect: Eff[R, A])(handle: NonEmptyList[E] => Eff[R, A])(using Validate[E, *] <= R): Eff[R, A] =
    catchWrongs(effect)(handle)

}

object ValidateInterpretation extends ValidateInterpretation
