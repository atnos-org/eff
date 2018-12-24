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
case class Warning[E](e: E) extends Validate[E, Unit]
case class Wrong[E](e: E) extends Validate[E, Unit]

trait ValidateCreation {

  /** create an Validate effect from a single Option value */
  def validateOption[R, E, A](option: Option[A], e: => E)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    option.map(_ => correct(())).getOrElse(wrong(e))

  /** create an Validate effect from a single Either value */
  def validateEither[R, E, A](either: E Either A)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    either.fold(e => wrong(e), _ => correct(()))

  /** create an Validate effect from a single Ior value */
  def validateIor[R, E, A](ior: E Ior A)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    ior.fold(e => wrong(e), _ => correct(()), (w, _) => warning(w))

  /** create a failed value */
  def wrong[R, E](e: E)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    send[Validate[E, ?], R, Unit](Wrong(e))

  /** create a correct value */
  def correct[R, E, A](a: A)(implicit m: Validate[E, ?] |= R): Eff[R, A] =
    send[Validate[E, ?], R, Unit](Correct[E]()) >> Eff.EffMonad[R].pure(a)

  /** create a pure warning */
  def warning[R, E](e: E)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    send[Validate[E, ?], R, Unit](Warning[E](e))

  /** create a correct value with warning */
  def warning[R, E, A](a: A, e: E)(implicit m: Validate[E, ?] |= R): Eff[R, A] =
    warning(e) >> Eff.EffMonad[R].pure(a)

  /** check a correct condition */
  def validateCheck[R, E](condition: Boolean, e: => E)(implicit m: Validate[E, ?] |= R): Eff[R, Unit] =
    if (condition) correct(()) else wrong(e)

  /** check a correct value */
  def validateValue[R, E, A](condition: Boolean, a: => A, e: => E)(implicit m: Validate[E, ?] |= R): Eff[R, A] =
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
  def runMap[R, U, E, L : Semigroup, A](effect: Eff[R, A])(map: E => L)(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, L Either A] =
    runMapGen(effect)(map) { (a, l) => l.map(_ => a) }

  /** run the validate effect, yielding a non-empty list of failures or A or both */
  def runIorNel[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, E IorNel A] =
    runIorMap[R, U, E, NonEmptyList[E], A](r)((e: E) => NonEmptyList.one(e))

  /** run the validate effect, yielding a list of failures or A or both */
  def runIorMap[R, U, E, L : Semigroup, A](effect: Eff[R, A])(map: E => L)(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, L Ior A] =
    runMapGen(effect)(map) { (a, l) => l match {
      case Left(errs) => Ior.Left(errs)
      case Right(None) => Ior.Right(a)
      case Right(Some(warns)) => Ior.Both(warns, a)
    }}

  private def runMapGen[R, U, E, L : Semigroup, A, SomeOr[_, _]](effect: Eff[R, A])(map: E => L)(pure: (A, L Either Option[L]) => L SomeOr A)(implicit m: Member.Aux[Validate[E, ?], R, U]): Eff[U, L SomeOr A] =
    runInterpreter(effect)(new Interpreter[Validate[E, ?], U, A, L SomeOr A] {
      // Left means failed, Right means not failed (Option contains warnings)
      private var l: L Either Option[L] = Right(None)

      def onPure(a: A): Eff[U, L SomeOr A] =
        Eff.pure(pure(a, l))

      private def combineLV[X](l: L Either Option[L], v: Validate[E, X]): L Either Option[L] = v match {
        case Correct() => l
        case Warning(w) => l match {
          case Left(errs) => Left(errs |+| map(w))
          case Right(None) => Right(Some(map(w)))
          case Right(Some(warns)) => Right(Some(warns |+| map(w)))
        }
        case Wrong(e) => l match {
          case Left(errs) => Left(errs |+| map(e))
          case Right(None) => Left(map(e))
          case Right(Some(warns)) => Left(warns |+| map(e)) // uniting warnings and errors as cats do
        }
      }

      def onEffect[X](v: Validate[E, X], continuation: Continuation[U, X, L SomeOr A]): Eff[U, L SomeOr A] = {
        l = combineLV(l, v)
        Eff.impure(().asInstanceOf[X], continuation)
      }

      def onLastEffect[X](x: Validate[E, X], continuation: Continuation[U, X, Unit]): Eff[U, Unit] =
        Eff.pure(())

      def onApplicativeEffect[X, T[_] : Traverse](xs: T[Validate[E, X]], continuation: Continuation[U, T[X], L SomeOr A]): Eff[U, L SomeOr A] = {
        l = xs.foldLeft(l)(combineLV)

        val tx: T[X] = xs.map { case Correct() | Warning(_) | Wrong(_) => () }
        Eff.impure(tx, continuation)
      }
    })

  /** catch and handle possible wrong values */
  def catchWrongs[R, E, A, S[_]: Applicative](effect: Eff[R, A])(handle: S[E] => Eff[R, A])(implicit member: Validate[E, ?] <= R, semi: Semigroup[S[E]]): Eff[R, A] =
    intercept(effect)(new Interpreter[Validate[E, ?], R, A, A] {
      private var errs: Option[S[E]] = None

      def onPure(a: A): Eff[R, A] =
        errs.map(handle).getOrElse(Eff.pure(a))

      def onEffect[X](m: Validate[E, X], continuation: Continuation[R, X, A]): Eff[R, A] = {
        val x: X = m match {
          case Correct() | Warning(_) => ()
          case Wrong(e)               => {
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
          case Correct() | Warning(_) => (None, ())
          case Wrong(e)               => (Some(Applicative[S].pure(e)), ())
        }

        errs = errs |+| eo
        Eff.impure(tx, continuation)
      }
    })

  /** catch and handle the first wrong value */
  def catchFirstWrong[R, E, A](effect: Eff[R, A])(handle: E => Eff[R, A])(implicit member: Validate[E, ?] <= R): Eff[R, A] = {
    implicit val first: Semigroup[E] = Semigroup.instance{ (a, _) => a }
    catchWrongs[R, E, A, Id](effect)(handle)
  }

  /** catch and handle the last wrong value */
  def catchLastWrong[R, E, A](effect: Eff[R, A])(handle: E => Eff[R, A])(implicit member: Validate[E, ?] <= R): Eff[R, A] = {
    implicit val last: Semigroup[E] = Semigroup.instance{ (_, b) => b }
    catchWrongs[R, E, A, Id](effect)(handle)
  }

  /** catch and handle all wrong values */
  def catchAllWrongs[R, E, A](effect: Eff[R, A])(handle: NonEmptyList[E] => Eff[R, A])(implicit member: Validate[E, ?] <= R): Eff[R, A] =
    catchWrongs(effect)(handle)

  /** catch and handle possible wrong values */
  @deprecated("Use catchFirstWrong or more general catchWrongs instead", "5.4.2")
  def catchWrong[R, E, A](effect: Eff[R, A])(handle: E => Eff[R, A])(implicit member: (Validate[E, ?]) <= R): Eff[R, A] =
    catchFirstWrong(effect)(handle)
}

object ValidateInterpretation extends ValidateInterpretation

