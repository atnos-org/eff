package org.atnos.eff
package addon

import _root_.scalaz._

package object scalaz {

  /**
   * Monad implementation for the Eff[R, *] type
   */
  implicit final def EffScalazMonad[R]: Monad[Eff[R, *]] with BindRec[Eff[R, *]] = new Monad[Eff[R, *]] with BindRec[Eff[R, *]] {
    def point[A](a: => A): Eff[R, A] =
      Eff.EffMonad[R].pure(a)

    override def map[A, B](fa: Eff[R, A])(f: A => B): Eff[R, B] =
      Eff.EffMonad[R].map(fa)(f)

    def bind[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      Eff.EffMonad[R].flatMap(fa)(f)

    def tailrecM[A, B](a: A)(f: A => Eff[R, A \/ B]): Eff[R, B] =
      Eff.EffMonad[R].tailRecM(a)(a1 => f(a1).map(_.toEither))
  }

  def EffScalazApplicative[R]: Applicative[Eff[R, *]] = new Applicative[Eff[R, *]] {
    def point[A](a: => A): Eff[R, A] =
      Eff.EffApplicative[R].pure(a)

    def ap[A, B](fa: => Eff[R, A])(ff: => Eff[R, A => B]): Eff[R, B] =
      Eff.EffApplicative[R].ap(ff)(fa)
  }

  def catsSemigroup[A](s: Semigroup[A]): cats.Semigroup[A] =
    (x: A, y: A) => s.append(x, y)

}
