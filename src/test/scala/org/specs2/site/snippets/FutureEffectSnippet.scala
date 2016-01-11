// 8<---
package org.specs2.site.snippets

import cats.data.Xor
import cats.data.Xor.Left
import org.specs2.control.eff.Interpret._
import org.specs2.control.eff.{Member, Effects, Eff}
import org.specs2.control.eff.Eff._
import org.specs2.control.eff.Member._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

trait FutureEffectSnippet {

// 8<---
object FutureEffect {
  type Fut[A] = Future[() => A]

  def future[R, A](a: => A)(implicit m: Fut <= R): Eff[R, A] =
    send[Fut, R, A](Future(() => a))

  def runFuture[R <: Effects, U <: Effects, A, B](atMost: Duration)(effects: Eff[R, A])(
     implicit m: Member.Aux[Fut, R, U]): Eff[U, A] = {

    val recurse = new Recurse[Fut, U, A] {
      def apply[X](m: Fut[X]): X Xor Eff[U, A] =
        Left(Await.result(m.map(_ ()), atMost))
    }
    interpret1((a: A) => a)(recurse)(effects)(m)
  }
}

// 8<---
}

object FutureEffectSnippet extends FutureEffectSnippet

