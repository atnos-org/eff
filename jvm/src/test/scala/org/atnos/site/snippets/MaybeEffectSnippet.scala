// 8<---
package org.atnos.site.snippets

trait MaybeEffectSnippet {

// 8<---

import cats._, implicits._
import org.atnos.eff._
import all._
import org.atnos.eff.interpret._

sealed trait Maybe[A]
case class Just[A](a: A) extends Maybe[A]
case class Nothing[A]() extends Maybe[A]

object MaybeEffect {
  type _maybe[R] = Maybe |= R

  def just[R :_maybe, A](a: A): Eff[R, A] =
    send[Maybe, R, A](Just(a))

  def nothing[R :_maybe, A]: Eff[R, A] =
    send[Maybe, R, A](Nothing())

  def runMaybe[R, U, A, B](effect: Eff[R, A])(implicit m: Member.Aux[Maybe, R, U]): Eff[U, Option[A]] =
    recurse(effect)(new Recurser[Maybe, U, A, Option[A]] {
      def onPure(a: A) = Some(a)

      def onEffect[X](m: Maybe[X]): X Either Eff[U, Option[A]] =
        m match {
          case Just(x)   => Left(x)
          case Nothing() => Right(Eff.pure(None))
        }

      def onApplicative[X, T[_]: Traverse](ms: T[Maybe[X]]): T[X] Either Maybe[T[X]] =
        Right(ms.sequence)
    })

  implicit val applicativeMaybe: Applicative[Maybe] = new Applicative[Maybe] {
    def pure[A](a: A): Maybe[A] = Just(a)

    def ap[A, B](ff: Maybe[A => B])(fa: Maybe[A]): Maybe[B] =
      (fa, ff) match {
        case (Just(a), Just(f)) => Just(f(a))
        case _                  => Nothing()
      }
  }
}

// 8<---
}

object MaybeEffectSnippet extends MaybeEffectSnippet

