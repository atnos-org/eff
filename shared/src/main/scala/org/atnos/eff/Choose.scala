package org.atnos.eff

import Eff._
import cats.{Alternative, MonadCombine}
import cats.syntax.cartesian._
import cats.syntax.functor._

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
  ChooseInterpretation

object ChooseEffect extends ChooseEffect

trait ChooseCreation {

  type _Choose[R] = Choose <= R
  type _choose[R] = Choose |= R

  def zero[R :_choose, A]: Eff[R, A] =
    send[Choose, R, A](ChooseZero[A]())

  def plus[R :_choose, A](a1: Eff[R, A], a2: Eff[R, A]): Eff[R, A] =
    EffMonad[R].flatMap(send(ChoosePlus))((b: Boolean) => if (b) a1 else a2)

  def chooseFrom[R :_choose, A](as: List[A]): Eff[R, A] =
    as match {
      case Nil => send[Choose, R, A](ChooseZero[A]())
      case a :: rest => plus(EffMonad[R].pure(a), chooseFrom(rest))
    }
}

object ChooseCreation extends ChooseCreation

trait ChooseInterpretation {
  def runChoose[R, U, A, F[_] : Alternative](r: Eff[R, A])(implicit m: Member.Aux[Choose, R, U]): Eff[U, F[A]] = {
    def lastRun(l: Last[R]): Last[U] = Last.eff(runChoose[R, U, Unit, F](l.run).as(()))

    r match {
      case Pure(a, last) =>
        EffMonad[U].pure(Alternative[F].pure(a)).addLast(lastRun(last))

      case Impure(u, c, last) =>
        m.project(u) match {
          case Left(u1) =>
            Impure(u1, Arrs.singleton((x: u1.X) => runChoose(c(x)))).addLast(lastRun(last))

          case Right(choose) =>
            choose match {
              case ChooseZero() => EffMonad[U].pure(Alternative[F].empty)
              case _ =>
                val continuation = c.asInstanceOf[Arrs[R, Boolean, A]]
                (runChoose(continuation(true)) |@| runChoose(continuation(false))).map(Alternative[F].combineK)
            }
        }

      case ap @ ImpureAp(_, _, _) =>
        runChoose(ap.toMonadic)
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

    def tailRecM[A, B](a: A)(f: A => Eff[R, Either[A, B]]): Eff[R, B] =
      flatMap(f(a)) {
        case Right(b)   => pure(b)
        case Left(next) => tailRecM(next)(f)
      }

    def empty[A]: Eff[R, A] =
      ChooseEffect.zero[R, A]

    def combineK[A](a1: Eff[R, A], a2: Eff[R, A]): Eff[R, A] =
      ChooseEffect.plus(a1, a2)
  }

}

object ChooseImplicits extends ChooseImplicits
