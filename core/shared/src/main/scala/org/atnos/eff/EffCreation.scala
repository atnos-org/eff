package org.atnos.eff

import cats._
import cats.syntax.all._
import Eff._
import scala.concurrent.duration.FiniteDuration

trait EffCreation {

  /** create an Eff[R, A] value from an effectful value of type T[V] provided that T is one of the effects of R */
  def send[T[_], R, V](tv: T[V])(implicit member: T |= R): Eff[R, V] =
    ImpureAp(Unions(member.inject(tv), Vector.empty), Continuation.lift(xs => pure[R, V](xs.head.asInstanceOf[V])))

  /** use the internal effect as one of the stack effects */
  def collapse[R, M[_], A](r: Eff[R, M[A]])(implicit m: M |= R): Eff[R, A] =
    Monad[Eff[R, *]].flatMap(r)(mx => send(mx)(using m))

  /** create an Eff value for () */
  def unit[R]: Eff[R, Unit] =
    Monad[Eff[R, *]].pure(())

  /** create a pure value */
  def pure[R, A](a: A): Eff[R, A] =
    Pure(a)

  /** create a impure value from an union of effects and a continuation */
  def impure[R, X, A](union: Union[R, X], continuation: Continuation[R, X, A]): Eff[R, A] =
    Impure[R, X, A](union, continuation)

  /** create a delayed impure value */
  def impure[R, A, B](value: A, continuation: Continuation[R, A, B]): Eff[R, B] =
    Impure(NoEffect(value), continuation)

  /** create a delayed impure value */
  def impure[R, A, B](value: A, continuation: Continuation[R, A, B], map: B => B): Eff[R, B] =
    Impure(NoEffect(value), Continuation.lift((a: A) => continuation(a), continuation.onNone).map(map))

  /** apply a function to an Eff value using the applicative instance */
  def ap[R, A, B](a: Eff[R, A])(f: Eff[R, A => B]): Eff[R, B] =
    EffImplicits.EffApplicative[R].ap(f)(a)

  /** use the applicative instance of Eff to traverse a list of values */
  def traverseA[R, F[_]: Traverse, A, B](fs: F[A])(f: A => Eff[R, B]): Eff[R, F[B]] =
    Traverse[F].traverse(fs)(f)(using EffImplicits.EffApplicative[R])

  /** use the applicative instance of Eff to sequence a list of values */
  def sequenceA[R, F[_]: Traverse, A](fs: F[Eff[R, A]]): Eff[R, F[A]] =
    Traverse[F].sequence(fs)(using EffImplicits.EffApplicative[R])

  /** use the applicative instance of Eff to traverse a list of values, then flatten it */
  def flatTraverseA[R, F[_], A, B](fs: F[A])(f: A => Eff[R, F[B]])(implicit FT: Traverse[F], FM: FlatMap[F]): Eff[R, F[B]] =
    FT.flatTraverse[Eff[R, *], A, B](fs)(f)(using EffImplicits.EffApplicative[R], FM)

  /** use the applicative instance of Eff to sequence a list of values, then flatten it */
  def flatSequenceA[R, F[_], A](fs: F[Eff[R, F[A]]])(implicit FT: Traverse[F], FM: FlatMap[F]): Eff[R, F[A]] =
    FT.flatSequence[Eff[R, *], A](fs)(using EffImplicits.EffApplicative[R], FM)

  /** bracket an action with one last action to execute at the end of the program */
  def bracketLast[R, A, B, C](acquire: Eff[R, A])(use: A => Eff[R, B])(release: A => Eff[R, C]): Eff[R, B] =
    for {
      a <- acquire
      b <- use(a).addLast(release(a).void)
    } yield b

  /** attach a clean-up action to the continuation (if any) */
  def whenStopped[R, A](e: Eff[R, A], action: Last[R]): Eff[R, A] =
    e match {
      case x @ Pure(_, _) => x
      case Impure(u, c, l) => Impure(u, c.copy(onNone = c.onNone <* action), l)
      case ImpureAp(u, c, l) => ImpureAp(u, c.copy(onNone = c.onNone <* action), l)
    }

  def retryUntil[R, A](e: Eff[R, A], condition: A => Boolean, durations: List[FiniteDuration], waitFor: FiniteDuration => Eff[R, Unit]): Eff[R, A] =
    e.flatMap { a =>
      if (condition(a))
        Eff.pure(a)
      else
        durations match {
          case Nil =>
            Eff.pure(a)

          case duration :: rest =>
            waitFor(duration) >>
              retryUntil(e, condition, rest, waitFor)
        }
    }

}

object EffCreation extends EffCreation
