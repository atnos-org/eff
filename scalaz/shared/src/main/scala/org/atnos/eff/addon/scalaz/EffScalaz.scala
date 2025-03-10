package org.atnos.eff
package addon
package scalaz

import _root_.scalaz.*
import _root_.scalaz.std.vector.*
import _root_.scalaz.syntax.functor.*

object EffScalaz {

  def traverseA[R, F[_]: Traverse, A, B](fs: F[A])(f: A => Eff[R, B]): Eff[R, F[B]] =
    Traverse[F].traverse(fs)(f)(using EffScalazApplicative[R])

  def sequenceA[R, F[_]: Traverse, A](fs: F[Eff[R, A]]): Eff[R, F[A]] =
    Traverse[F].sequence(fs)(using EffScalazApplicative[R])

  def flatTraverseA[R, F[_], A, B](fs: F[A])(
    f: A => Eff[R, F[B]]
  )(using FT: Traverse[F], FM: Bind[F]): Eff[R, F[B]] =
    FT.traverseM[A, Eff[R, *], B](fs)(f)(using EffScalazApplicative[R], FM)

  /** use the applicative instance of Eff to sequence a list of values, then flatten it */
  def flatSequenceA[R, F[_], A](fs: F[Eff[R, F[A]]])(using FT: Traverse[F], FM: Bind[F]): Eff[R, F[A]] =
    FT.traverseM[Eff[R, F[A]], Eff[R, *], A](fs)(identity)(using EffScalazApplicative[R], FM)

  def detach[M[_], A](eff: Eff[Fx1[M], A])(using Monad[M], BindRec[M]): M[A] =
    BindRec[M].tailrecM[Eff[Fx1[M], A], A](eff) {
      case Pure(a, Last(Some(l))) => Monad[M].point(-\/(l.value.as(a)))
      case Pure(a, Last(None)) => Monad[M].point(\/-(a))

      case Impure(NoEffect(a), continuation, last) =>
        Monad[M].point(-\/(continuation(a).addLast(last)))

      case Impure(u, continuation, last) =>
        u match {
          case NoEffect(a) => Monad[M].point(-\/(continuation(a).addLast(last)))
          case UnionTagged(ta: M[Nothing] @unchecked, _) =>
            last match {
              case Last(Some(_)) => Monad[M].map(ta)(x => -\/(continuation(x).addLast(last)))
              case Last(None) => Monad[M].map(ta)(x => -\/(continuation(x)))
            }
        }

      case ap @ ImpureAp(_, _, _) =>
        Monad[M].point(-\/(ap.toMonadic))
    }

  def detachA[M[_], A](
    eff: Eff[Fx1[M], A]
  )(using monad: Monad[M], bindRec: BindRec[M], applicative: Applicative[M]): M[A] =
    BindRec[M].tailrecM[Eff[Fx1[M], A], A](eff) {
      case Pure(a, Last(Some(l))) => monad.point(-\/(l.value.as(a)))
      case Pure(a, Last(None)) => monad.point(\/-(a))

      case Impure(NoEffect(a), continuation, last) =>
        monad.point(-\/(continuation(a).addLast(last)))

      case Impure(u, continuation, last) =>
        u match {
          case NoEffect(a) => Monad[M].point(-\/(continuation(a).addLast(last)))
          case UnionTagged(ta: M[Nothing] @unchecked, _) =>
            last match {
              case Last(Some(_)) => Monad[M].map(ta)(x => -\/(continuation(x).addLast(last)))
              case Last(None) => Monad[M].map(ta)(x => -\/(continuation(x)))
            }
        }

      case ImpureAp(unions, continuation, last) =>
        val effects = unions.unions.collect { case UnionTagged(mx: M[Nothing] @unchecked, _) => mx }
        val sequenced = applicative.sequence[Nothing, Vector](effects)

        last match {
          case Last(Some(_)) => Monad[M].map(sequenced)(x => -\/(continuation(x).addLast(last)))
          case Last(None) => Monad[M].map(sequenced)(x => -\/(continuation(x)))
        }
    }
}
