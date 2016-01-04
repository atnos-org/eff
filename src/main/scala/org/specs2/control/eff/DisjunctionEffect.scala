package org.specs2.control.eff

import Eff._
import Effects._
import Interpret._

import cats.data.Xor, Xor._
import cats.syntax.functor._

/**
 * Effect for computation which can fail
 */
object DisjunctionEffect {

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
    send[E Xor ?, R, A](Left(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
    send[E Xor ?, R, A](Right(a))

  /** run the disjunction effect, yielding E Xor A */
  def runDisjunction[R <: Effects, E, A](r: Eff[(E Xor ?) |: R, A]): Eff[R, E Xor A] = {
    val recurse = new Recurse[(E Xor ?), R, E Xor A] {
      def apply[X](m: E Xor X) =
        m match {
          case Left(e) => Right(EffMonad[R].pure(Left(e)))
          case Right(a) => Left(a)
        }
    }

    interpret1[R, (E Xor ?), A, E Xor A]((a: A) => Right(a))(recurse)(r)
  }

  /** run the disjunction effect, yielding Either[E, A] */
  def runDisjunctionEither[R <: Effects, E, A](r: Eff[(E Xor ?) |: R, A]): Eff[R, Either[E, A]] =
    runDisjunction(r).map(_.fold(util.Left.apply, util.Right.apply))

}
