package org.atnos.eff

import cats._, data._, Xor._
import cats.implicits._
import Eff._
import Interpret._

/**
 * Effect for computation which can fail
 */
trait XorEffect extends
  XorCreation with
  XorInterpretation

object XorEffect extends XorEffect

trait XorCreation {

  type ThrowableXor[A] = Throwable Xor A
  type _ThrowableXor[R] = ThrowableXor <= R
  type _throwableXor[R] = ThrowableXor |= R

  /** create an Xor effect from a single Option value */
  def optionXor[R, E, A](option: Option[A], e: E)(implicit member: (E Xor ?) |= R): Eff[R, A] =
    option.fold[Eff[R, A]](left[R, E, A](e))(right[R, E, A])

  /** create an Xor effect from a single Xor value */
  def fromXor[R, E, A](xor: E Xor A)(implicit member: (E Xor ?) |= R): Eff[R, A] =
    xor.fold[Eff[R, A]](left[R, E, A], right[R, E, A])

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: (E Xor ?) |= R): Eff[R, A] =
    send[E Xor ?, R, A](Left(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: (E Xor ?) |= R): Eff[R, A] =
    send[E Xor ?, R, A](Right(a))

}

object XorCreation extends XorCreation

trait XorInterpretation {

  /** run the xor effect, yielding E Xor A */
  def runXor[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E Xor ?), R, U]): Eff[U, E Xor A] = {
    val recurse = new Recurse[(E Xor ?), U, E Xor A] {
      def apply[X](m: E Xor X) =
        m match {
          case Left(e) => Right(EffMonad[U].pure(Left(e)))
          case Right(a) => Left(a)
        }

      def applicative[X, T[_] : Traverse](ms: T[E Xor X]): T[X] Xor (E Xor T[X]) =
        Xor.Right(ms.sequence)
    }

    interpret1[R, U, (E Xor ?), A, E Xor A]((a: A) => Right(a): E Xor A)(recurse)(r)
  }

  /** run the xor effect, yielding Either[E, A] */
  def runEither[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E Xor ?), R, U]): Eff[U, Either[E, A]] =
    runXor(r).map(_.fold(util.Left.apply, util.Right.apply))

  /** catch and handle a possible left value */
  def catchLeft[R, E, A](r: Eff[R, A])(handle: E => Eff[R, A])(implicit member: (E Xor ?) <= R): Eff[R, A] = {
    val recurse = new Recurse[(E Xor ?), R, A] {
      def apply[X](m: E Xor X) =
        m match {
          case Left(e) => Right(handle(e))
          case Right(a) => Left(a)
        }

      def applicative[X, T[_]: Traverse](ms: T[E Xor X]): T[X] Xor (E Xor T[X]) =
        Xor.Right(ms.sequence)
    }

    intercept1[R, (E Xor ?), A, A]((a: A) => a)(recurse)(r)
  }

  /**
   * Lift a computation over a "small" error (for a subsystem) into
   * a computation over a "bigger" error (for the full application)
   */
  def localXor[SR, BR, U, E1, E2, A](r: Eff[SR, A], getter: E1 => E2)
                                    (implicit sr: Member.Aux[E1 Xor ?, SR, U],
                                       br: Member.Aux[E2 Xor ?, BR, U]): Eff[BR, A] =
    transform[SR, BR, U, E1 Xor ?, E2 Xor ?, A](r,
      new ~>[E1 Xor ?, E2 Xor ?] {
        def apply[X](r: E1 Xor X): E2 Xor X =
          r.leftMap(getter)
      })

  /**
   * Translate an error effect to another one in the same stack
   * a computation over a "bigger" error (for the full application)
   */
  def runLocalXor[R, U, E1, E2, A](r: Eff[R, A], getter: E1 => E2)
                                  (implicit sr: Member.Aux[E1 Xor ?, R, U], br: (E2 Xor ?) |= U): Eff[U, A] =
    translate(r) { new Translate[E1 Xor ?, U] {
      def apply[X](ex: E1 Xor X): Eff[U, X] =
        ex match {
          case Xor.Left(e1) => xor.left(getter(e1))
          case Xor.Right(x) => pure(x)
        }
    }}

}

object XorInterpretation extends XorInterpretation
