package org.atnos.eff
package syntax

import cats.*

/**
  * Operations of Eff[R, A] values
  */
object eff extends eff

trait eff extends effOperations with effCats

trait effOperations {
  given effExtension: AnyRef with {
    extension [A](e: Eff[NoFx, A]) {
      def run: A =
        Eff.run(e)
    }

    extension [M[_], A](ma: M[A]) {
      def send[R](using M |= R): Eff[R, A] = Eff.send(ma)
    }

    extension [A](a: A) {
      def pureEff[R]: Eff[R, A] =
        Eff.pure(a)
    }

  }
}

trait effCats {
  given effCatsExtension: AnyRef with {
    extension [M[_], A](e: Eff[Fx1[M], A]) {
      def detach[E](using MonadError[M, E]): M[A] =
        Eff.detach(e)

      def detachA[E](applicative: Applicative[M])(using monad: MonadError[M, E]): M[A] =
        Eff.detachA(e)(using monad, applicative)
    }

    extension [R, M[_], A](e: Eff[R, M[A]]) {
      def collapse(using M |= R): Eff[R, A] =
        Eff.collapse[R, M, A](e)
    }

    extension [M[_], A](ma: M[A]) {
      def traverseA[R, B](f: A => Eff[R, B])(using Traverse[M]): Eff[R, M[B]] =
        Eff.traverseA(ma)(f)

      def flatTraverseA[R, B](f: A => Eff[R, M[B]])(using Traverse[M], FlatMap[M]): Eff[R, M[B]] =
        Eff.flatTraverseA(ma)(f)
    }

    extension [F[_], R, A](values: F[Eff[R, A]]) {
      def sequenceA(using Traverse[F]): Eff[R, F[A]] =
        Eff.sequenceA(values)
    }

    extension [F[_], R, A](values: F[Eff[R, F[A]]]) {
      def flatSequenceA(using Traverse[F], FlatMap[F]): Eff[R, F[A]] =
        Eff.flatSequenceA(values)
    }

    extension [R, A](e: Eff[R, A]) {
      def tuple2[B](b: Eff[R, B]): Eff[R, (A, B)] =
        Eff.EffApplicative[R].tuple2(e, b)
    }
  }
}
