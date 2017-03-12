package org.atnos.eff
package addon

import org.atnos.eff.addon.scalaz.concurrent.TaskEffect

import _root_.scalaz._
import Scalaz._

package object scalaz {

  object all extends
    either with
    eval   with
    safe   with
    validate

  object task extends TaskEffect

  /**
   * Monad implementation for the Eff[R, ?] type
   */
  implicit final def EffScalazMonad[R]: Monad[Eff[R, ?]] with BindRec[Eff[R, ?]] = new Monad[Eff[R, ?]] with BindRec[Eff[R, ?]] {
    def point[A](a: =>A): Eff[R, A] =
      Eff.EffMonad[R].pure(a)

    override def map[A, B](fa: Eff[R, A])(f: A => B): Eff[R, B] =
      Eff.EffMonad[R].map(fa)(f)

    def bind[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      Eff.EffMonad[R].flatMap(fa)(f)

    def tailrecM[A, B](f: A => Eff[R, A \/ B])(a: A): Eff[R, B] =
      Eff.EffMonad[R].tailRecM(a)(a1 => f(a1).map(_.fold(Left.apply, Right.apply)))
  }

  def EffScalazApplicative[R]: Applicative[Eff[R, ?]] = new Applicative[Eff[R, ?]] {
    def point[A](a: =>A): Eff[R, A] =
      Eff.EffApplicative[R].pure(a)

    def ap[A, B](fa: =>Eff[R, A])(ff: =>Eff[R, A => B]): Eff[R, B] =
      Eff.EffApplicative[R].ap(ff)(fa)
  }

  def catsSemigroup[A](s: Semigroup[A]): cats.Semigroup[A] = new cats.Semigroup[A] {
    def combine(x: A, y: A): A = s.append(x, y)
  }

  object EffScalaz {
    def traverseA[R, F[_] : Traverse, A, B](fs: F[A])(f: A => Eff[R, B]): Eff[R, F[B]] =
      Traverse[F].traverse(fs)(f)(EffScalazApplicative[R])

    def sequenceA[R, F[_] : Traverse, A](fs: F[Eff[R, A]]): Eff[R, F[A]] =
      Traverse[F].sequence(fs)(EffScalazApplicative[R])

    def flatTraverseA[R, F[_], A, B](fs: F[A])(f: A => Eff[R, F[B]])(implicit FT: Traverse[F], FM: Bind[F]): Eff[R, F[B]] =
      FT.traverseM[A, Eff[R, ?], B](fs)(f)(EffScalazApplicative[R], FM)

    /** use the applicative instance of Eff to sequence a list of values, then flatten it */
    def flatSequenceA[R, F[_], A](fs: F[Eff[R, F[A]]])(implicit FT: Traverse[F], FM: Bind[F]): Eff[R, F[A]] =
      FT.traverseM[Eff[R, F[A]], Eff[R, ?], A](fs)(identity)(EffScalazApplicative[R], FM)

    def detach[M[_], A](eff: Eff[Fx1[M], A])(implicit m: Monad[M], b: BindRec[M]): M[A] =
      BindRec[M].tailrecM[Eff[Fx1[M], A], A] {
        case Pure(a, Last(Some(l))) => Monad[M].point(-\/(l.value.as(a)))
        case Pure(a, Last(None))    => Monad[M].point(\/-(a))

        case Impure(NoEffect(a), continuation, last) =>
          Monad[M].point(-\/(continuation(a).addLast(last)))

        case Impure(u, continuation, last) =>
          u match {
            case NoEffect(a) => Monad[M].point(-\/(continuation(a).addLast(last)))
            case UnionTagged(ta: M[Nothing] @unchecked, _) =>
              last match {
                case Last(Some(l)) => Monad[M].map(ta)(x => -\/(continuation(x).addLast(last)))
                case Last(None)    => Monad[M].map(ta)(x => -\/(continuation(x)))
              }
          }

        case ap @ ImpureAp(_, _, _) =>
          Monad[M].point(-\/(ap.toMonadic))
      }(eff)

    def detachA[M[_], A](eff: Eff[Fx1[M], A])(implicit monad: Monad[M], bindRec: BindRec[M], applicative: Applicative[M]): M[A] =
      BindRec[M].tailrecM[Eff[Fx1[M], A], A] {
        case Pure(a, Last(Some(l))) => monad.point(-\/(l.value.as(a)))
        case Pure(a, Last(None))    => monad.point(\/-(a))

        case Impure(NoEffect(a), continuation, last) =>
          monad.point(-\/(continuation(a).addLast(last)))

        case Impure(u, continuation, last) =>
          u match {
            case NoEffect(a) => Monad[M].point(-\/(continuation(a).addLast(last)))
            case UnionTagged(ta: M[Nothing] @unchecked, _) =>
              last match {
                case Last(Some(l)) => Monad[M].map(ta)(x => -\/(continuation(x).addLast(last)))
                case Last(None)    => Monad[M].map(ta)(x => -\/(continuation(x)))
              }
          }

        case ap @ ImpureAp(unions, continuation, last) =>
          val effects = unions.unions.collect { case UnionTagged(mx: M[Nothing] @unchecked, _) => mx }
          val sequenced = applicative.sequence[Nothing, Vector](effects)

          last match {
            case Last(Some(l)) => Monad[M].map(sequenced)(x => -\/(continuation(x).addLast(last)))
            case Last(None)    => Monad[M].map(sequenced)(x => -\/(continuation(x)))
          }
      }(eff)
  }

}
