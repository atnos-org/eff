// 8<---
package org.atnos.site.snippets

import cats.data.Xor
import cats.Applicative
import cats.implicits._
import org.atnos.eff._
import all._
import org.atnos.eff.interpret._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

trait FutEffectSnippet {

// 8<---
import scala.concurrent.ExecutionContext.Implicits.global

object FutEffect {
  type Fut[A] = Future[() => A]
  type _fut[R] = Fut |= R

  def ApplicativeFut(implicit ec: ExecutionContext): Applicative[Fut] = new Applicative[Fut] {
    def pure[A](x: A): Fut[A] =
      Future.successful(() => x)

    def ap[A, B](ff: Fut[A => B])(fa: Fut[A]): Fut[B] =
      fa.zip(ff).map { case (a, f) => () => f()(a()) }
  }

  def fut[R :_fut, A](a: => A): Eff[R, A] =
    send[Fut, R, A](Future(() => a))

  def runFuture[R, U, A, B](atMost: Duration)(effects: Eff[R, A])(
     implicit m: Member.Aux[Fut, R, U]): Eff[U, A] = {

    val recurse = new Recurse[Fut, U, A] {
      def apply[X](m: Fut[X]): X Xor Eff[U, A] =
        Xor.Left(Await.result(m.map(_ ()), atMost))

      def applicative[X](ms: List[Fut[X]]): List[X] Xor Fut[List[X]] =
        Xor.Right(ApplicativeFut.sequence(ms))

    }
    interpret1((a: A) => a)(recurse)(effects)
  }
}

// 8<---
}

object FutEffectSnippet extends FutEffectSnippet

