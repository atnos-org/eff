package org.atnos.eff
package syntax

import cats.{Monad, Traverse}
import cats.arrow.NaturalTransformation
import org.atnos.eff.Effects.|:

/**
 * Operations of Eff[R, A] values
 */
object eff extends eff

trait eff {

  implicit class EffOps[R <: Effects, A](e: Eff[R, A]) {
    def into[U](implicit f: IntoPoly[R, U, A]): Eff[U, A] =
      Eff.effInto(e)(f)

    def transform[BR, U, M[_], N[_]](t: NaturalTransformation[M, N])(implicit m: Member.Aux[M, R, U], n: Member.Aux[N, BR, U]): Eff[BR, A] =
      Interpret.transform(e, t)(m, n)
  }

  implicit class EffNoEffectOps[A](e: Eff[NoEffect, A]) {
    def run: A =
      Eff.run(e)
  }

  implicit class EffOneEffectOps[M[_] : Monad, A](e: Eff[M |: NoEffect, A]) {
    def detach: M[A] =
      Eff.detach(e)
  }

  implicit class EffMonadicOps[R <: Effects, M[_], A](e: Eff[R, M[A]]) {
    def collapse(implicit m: M <= R): Eff[R, A] =
      Eff.collapse[R, M, A](e)
  }

  implicit class EffApplicativeOps[F[_] : Traverse, A](values: F[A]) {
    def traverseA[R, B](f: A => Eff[R, B]): Eff[R, F[B]] =
      Eff.traverseA(values)(f)
  }

  implicit class EffSequenceOps[F[_] : Traverse, R, A](values: F[Eff[R, A]]) {
    def sequenceA: Eff[R, F[A]] =
      Eff.sequenceA(values)
  }

  implicit class EffApplicativeSyntaxOps[R, A](a: Eff[R, A]) {
    def tuple2[B](b: Eff[R, B]): Eff[R, (A, B)] =
      Eff.EffApplicative[R].tuple2(a, b)
  }
}
