// 8<---
package org.atnos.site.snippets

import cats.{Applicative, Traverse}
import org.atnos.eff._
import all._
import org.atnos.eff.interpret._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

trait FutEffectSnippet {

// 8<---

object FutEffect {
  type Fut[A] = Future[() => A]
  type _fut[R] = Fut |= R

  def ApplicativeFut(implicit ec: ExecutionContext): Applicative[Fut] = new Applicative[Fut] {
    def pure[A](x: A): Fut[A] =
      Future.successful(() => x)

    def ap[A, B](ff: Fut[A => B])(fa: Fut[A]): Fut[B] =
      fa.zip(ff).map { case (a, f) => () => f()(a()) }
  }

  import scala.concurrent.ExecutionContext.Implicits.global

  def fut[R :_fut, A](a: => A): Eff[R, A] =
    send[Fut, R, A](Future(() => a))

  def runFuture[R, U, A, B](atMost: Duration)(effect: Eff[R, A])(implicit m: Member.Aux[Fut, R, U]): Eff[U, A] =
    recurse(effect)(new Recurser[Fut, U, A, A] {
      def onPure(a: A) = a

      def onEffect[X](m: Fut[X]): X Either Eff[U, A] =
        Left(Await.result(m.map(_ ()), atMost))

      def onApplicative[X, T[_]: Traverse](ms: T[Fut[X]]): T[X] Either Fut[T[X]] =
        Right(ApplicativeFut.sequence(ms))

    })
}

// 8<---
}

object FutEffectSnippet extends FutEffectSnippet

