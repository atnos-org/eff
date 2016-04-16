package org.atnos.eff


import cats.data._, Xor._
import cats.syntax.functor._
import Eff._
import Interpret._
import Effects.|:

/**
 * Effect for computation which can fail
 */
trait DisjunctionEffect extends
  DisjunctionCreation with
  DisjunctionInterpretation with
  DisjunctionImplicits

object DisjunctionEffect extends DisjunctionEffect

trait DisjunctionCreation {

  /** create a Disjunction effect from a single Option value */
  def fromOption[R, E, A](option: Option[A], e: E)(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
    option.fold[Eff[R, A]](left[R, E, A](e))(right[R, E, A])

  /** create a Disjunction effect from a single Xor value */
  def fromXor[R, E, A](xor: E Xor A)(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
    xor.fold[Eff[R, A]](left[R, E, A], right[R, E, A])

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
    send[E Xor ?, R, A](Left(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
    send[E Xor ?, R, A](Right(a))

}

object DisjunctionCreation extends DisjunctionCreation

trait DisjunctionInterpretation {

  /** run the disjunction effect, yielding E Xor A */
  def runXor[R <: Effects, U <: Effects, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E Xor ?), R, U]): Eff[U, E Xor A] = {
    val recurse = new Recurse[(E Xor ?), U, E Xor A] {
      def apply[X](m: E Xor X) =
        m match {
          case Left(e) => Right(EffMonad[U].pure(Left(e)))
          case Right(a) => Left(a)
        }
    }

    interpret1[R, U, (E Xor ?), A, E Xor A]((a: A) => Right(a): E Xor A)(recurse)(r)
  }

  /** run the disjunction effect, yielding Either[E, A] */
  def runEither[R <: Effects, U <: Effects, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E Xor ?), R, U]): Eff[U, Either[E, A]] =
    runXor(r).map(_.fold(util.Left.apply, util.Right.apply))

  /** catch and handle a possible left value */
  def catchLeft[R <: Effects, E, A](r: Eff[R, A])(handle: E => Eff[R, A])(implicit member: Member[(E Xor ?), R]): Eff[R, A] = {
    val recurse = new Recurse[(E Xor ?), R, A] {
      def apply[X](m: E Xor X) =
        m match {
          case Left(e) => Right(handle(e))
          case Right(a) => Left(a)
        }
    }

    intercept1[R, (E Xor ?), A, A]((a: A) => a)(recurse)(r)
  }

}

object DisjunctionInterpretation extends DisjunctionInterpretation

trait DisjunctionImplicits extends DisjunctionImplicitsLower {
  implicit def DisjunctionMemberZero[R, A]: Member.Aux[Xor[A, ?], Xor[A, ?] |: NoEffect, NoEffect] = {
    type T[X] = Xor[A, X]
    Member.zero[T]
  }

  implicit def DisjunctionMemberFirst[R <: Effects, A]: Member.Aux[Xor[A, ?], Xor[A, ?] |: R, R] = {
    type T[X] = Xor[A, X]
    Member.first[T, R]
  }
}

trait DisjunctionImplicitsLower {
  implicit def DisjunctionMemberSuccessor[O[_], R <: Effects, U <: Effects, A](implicit m: Member.Aux[Xor[A, ?], R, U]): Member.Aux[Xor[A, ?], O |: R, O |: U] = {
    type T[X] = Xor[A, X]
    Member.successor[T, O, R, U]
  }
}
object DisjunctionImplicits extends DisjunctionImplicits
