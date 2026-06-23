package org.atnos.eff

import cats.*

trait ChooseImplicits {

  /**
   * MonadCombine implementation for the Eff[R, *] type if R contains the Choose effect
   */
  def EffMonadAlternative[R](using Member[Choose, R]): Alternative[Eff[R, *]] = new Alternative[Eff[R, *]] with Monad[Eff[R, *]] {
    def pure[A](a: A): Eff[R, A] =
      Monad[Eff[R, *]].pure(a)

    def flatMap[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      Monad[Eff[R, *]].flatMap(fa)(f)

    def tailRecM[A, B](a: A)(f: A => Eff[R, Either[A, B]]): Eff[R, B] =
      flatMap(f(a)) {
        case Right(b) => pure(b)
        case Left(next) => tailRecM(next)(f)
      }

    def empty[A]: Eff[R, A] =
      ChooseEffect.zero[R, A]

    def combineK[A](a1: Eff[R, A], a2: Eff[R, A]): Eff[R, A] =
      ChooseEffect.plus(a1, a2)
  }

}

object ChooseImplicits extends ChooseImplicits
