package org.atnos.eff

import cats._
import EffCompat._

trait EffImplicits {

  /**
    * Monad implementation for the Eff[R, *] type
    */
  private[this] final val effMonadUnsafeImpl: Monad[Eff[AnyRef, *]] = new Monad[Eff[AnyRef, *]] {

    def pure[A](a: A): Eff[AnyRef, A] =
      Pure[AnyRef, A](a)

    override def map[A, B](fa: Eff[AnyRef, A])(f: A => B): Eff[AnyRef, B] =
      fa match {
        case Pure(a, l) =>
          Impure(NoEffect(a), Continuation.unit.map(f), l)

        case Impure(union, continuation, last) =>
          Impure(union, continuation map f, last)

        case ImpureAp(unions, continuations, last) =>
          ImpureAp(unions, continuations map f, last)
      }

    /**
      * When flatMapping the last action must still be executed after the next action
      */
    def flatMap[A, B](fa: Eff[AnyRef, A])(f: A => Eff[AnyRef, B]): Eff[AnyRef, B] =
      fa match {
        case Pure(a, l) =>
          Impure[AnyRef, A, B](NoEffect[AnyRef, A](a), Continuation.lift(x => f(x).addLast(l)))

        case Impure(union, continuation, last) =>
          Impure(union, continuation.append(f), last)

        case ImpureAp(unions, continuation, last) =>
          ImpureAp(unions, continuation.append(f), last)
      }

    def tailRecM[A, B](a: A)(f: A => Eff[AnyRef, Either[A, B]]): Eff[AnyRef, B] =
      f(a) match {
        case Pure(v, _) =>
          v match {
            case Left(a1) => tailRecM(a1)(f)
            case Right(b) => Pure(b)
          }
        case Impure(u, c, l) =>
          Impure(u, Continuation.lift((x: u.X) => c(x).flatMap(_.fold(a11 => tailRecM(a11)(f), b => pure(b)))), l)

        case ImpureAp(u, c, l) =>
          ImpureAp(u, Continuation.lift(x => c(x).flatMap(_.fold(a11 => tailRecM(a11)(f), b => pure(b)))), l)
      }
  }

  private[this] final val effApplicativeUnsafeImpl: Applicative[Eff[AnyRef, *]] = new Applicative[Eff[AnyRef, *]] {

    def pure[A](a: A): Eff[AnyRef, A] =
      Pure[AnyRef, A](a)

    override def product[A, B](fa: Eff[AnyRef, A], fb: Eff[AnyRef, B]): Eff[AnyRef, (A, B)] =
      ap(map(fb)(b => (a: A) => (a, b)))(fa)

    def ap[A, B](ff: Eff[AnyRef, A => B])(fa: Eff[AnyRef, A]): Eff[AnyRef, B] =
      fa match {
        case Pure(a, last) =>
          ff match {
            case Pure(f, last1) =>
              Pure(f(a), last1 *> last)
            case Impure(NoEffect(f), c, last1) =>
              Impure[AnyRef, Any, B](NoEffect[AnyRef, Any](f), c.append(f1 => pure(f1(a))).cast[Continuation[Object, Any, B]], c.onNone)
                .addLast(last1 *> last)
            case Impure(u: Union[?, ?], c: Continuation[AnyRef, Any, A => B], last1) =>
              ImpureAp(Unions(u, Vector.empty), c.dimapEff((x: Vector[Any]) => x.head)(_.map(_(a))), last1 *> last)
            case ImpureAp(u, c, last1) =>
              ImpureAp(u, c.map(_(a)), last1 *> last)
          }

        case Impure(NoEffect(a), c, last) =>
          ap(ff)(c(a).addLast(last))

        case Impure(u: Union[AnyRef, Any], c: Continuation[AnyRef, Any, A], last) =>
          ff match {
            case Pure(f, last1) =>
              ImpureAp(Unions(u, Vector.empty), c.contramap((x: Vector[Any]) => x.head).map(f), last1 *> last)
            case Impure(NoEffect(f), c1, last1) =>
              Impure(u, c.append(x => c1(f).map(_(x)))).addLast(last1 *> last)
            case Impure(u1: Union[?, ?], c1: Continuation[AnyRef, Any, A => B], last1) =>
              ImpureAp[AnyRef, Any, B](Unions(u, Vector(u1)), Continuation.lift(ls => ap(c1(ls(1)))(c(ls.head)), c.onNone), last1 *> last)
            case ImpureAp(u1, c1, last1) =>
              ImpureAp(Unions(u, u1.unions), Continuation.lift(ls => ap(c1(ls.drop(1)))(c(ls.head)), c.onNone), last1 *> last)
          }

        case ImpureAp(unions, c, last) =>
          ff match {
            case Pure(f, last1) =>
              ImpureAp(unions, c map f, last1 *> last)
            case Impure(NoEffect(f), c1, last1) =>
              ImpureAp(unions, c.append(x => c1(f).map(_(x)))).addLast(last1 *> last)
            case Impure(u: Union[?, ?], c1: Continuation[AnyRef, Any, A => B], last1) =>
              ImpureAp(Unions(unions.first, unions.rest :+ u), Continuation.lift(ls => ap(c1(ls.last))(c(ls.dropRight(1))), c.onNone), last1 *> last)
            case ImpureAp(u, c1, last1) =>
              ImpureAp(
                u append unions,
                Continuation.lift(
                  { xs =>
                    val usize = u.size
                    val (taken, dropped) = xs.splitAt(usize)
                    // don't recurse if the number of effects is too large
                    // this will ensure stack-safety on large traversals
                    // and keep enough concurrency on smaller traversals
                    if (xs.size > 10)
                      Eff.impure(taken, Continuation.lift((xs1: Vector[Any]) => ap(c1(xs1))(c(dropped)), c1.onNone))
                    else
                      ap(c1(taken))(c(dropped))
                  },
                  c.onNone
                ),
                last1 *> last
              )
          }

      }
  }

  implicit final def EffMonad[R]: Monad[Eff[R, *]] = effMonadUnsafeImpl.asInstanceOf[Monad[Eff[R, *]]]

  final def EffApplicative[R]: Applicative[Eff[R, *]] = effApplicativeUnsafeImpl.asInstanceOf[Applicative[Eff[R, *]]]

}

object EffImplicits extends EffImplicits
