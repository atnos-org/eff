package org.atnos.eff

import Eff._
import cats.{Alternative, MonadCombine}
import cats.data.Xor
import cats.syntax.cartesian._

sealed trait Choose[T]
case class ChooseZero[T]() extends Choose[T]
case object ChoosePlus extends Choose[Boolean]

/**
 * The Choose effect models non-determinism
 * So we can get results, either:
 *   - no results (when using ChooseZero)
 *   - the result for action1 or the result for action b (when using ChoosePlus)
 *
 * When running this effect we can "collect" the results with any
 * F which has an Alternative instance.
 *
 * For example if F is List then:
 *  - no results is the empty list
 *  - the result for a or b is List(a, b)
 *
 * If F is Option then:
 *  - no results is the None
 *  - the result for a or b is Some(a) or Some(b
 */
trait ChooseEffect extends
  ChooseCreation with
  ChooseInterpretation with
  ChooseImplicits

object ChooseEffect extends ChooseEffect

trait ChooseCreation {
  def zero[R, A](implicit m: Choose <= R): Eff[R, A] =
    send[Choose, R, A](ChooseZero[A]())

  def plus[R, A](a1: Eff[R, A], a2: Eff[R, A])(implicit m: Choose <= R): Eff[R, A] =
    EffMonad[R].flatMap(send(ChoosePlus))((b: Boolean) => if (b) a1 else a2)

  def chooseFrom[R, A](as: List[A])(implicit m: Choose <= R): Eff[R, A] =
    as match {
      case Nil => send[Choose, R, A](ChooseZero[A]())
      case a :: rest => plus(EffMonad[R].pure(a), chooseFrom(rest))
    }
}

object ChooseCreation extends ChooseCreation

trait ChooseInterpretation {
  def runChoose[R <: Effects, U <: Effects, A, F[_] : Alternative](r: Eff[R, A])(implicit m: Member.Aux[Choose, R, U]): Eff[U, F[A]] = {
    r match {
      case Pure(a) =>
        EffMonad[U].pure(Alternative[F].pure(a))

      case Impure(u, c) =>
        m.project(u) match {
          case Xor.Left(u1) =>
            Impure(u1, Arrs.singleton((x: u1.X) => runChoose(c(x))))

          case Xor.Right(choose) =>
            choose match {
              case ChooseZero() => EffMonad[U].pure(Alternative[F].empty)
              case _ =>
                val continuation = c.asInstanceOf[Arrs[R, Boolean, A]]
                (runChoose(continuation(true)) |@| runChoose(continuation(false))).map(Alternative[F].combineK)
            }
        }
    }
  }
}

object ChooseInterpretation extends ChooseInterpretation

trait ChooseImplicits {
  /**
   * MonadCombine implementation for the Eff[R, ?] type if R contains the Choose effect
   */
  def EffMonadCombine[R](implicit m: Member[Choose, R]): MonadCombine[Eff[R, ?]] = new MonadCombine[Eff[R, ?]] {
    def pure[A](a: A): Eff[R, A] =
      EffMonad[R].pure(a)

    def flatMap[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      EffMonad[R].flatMap(fa)(f)

    def empty[A]: Eff[R, A] =
      ChooseEffect.zero[R, A]

    def combineK[A](a1: Eff[R, A], a2: Eff[R, A]): Eff[R, A] =
      ChooseEffect.plus(a1, a2)
  }

}

object ChooseImplicits extends ChooseImplicits
