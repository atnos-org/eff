package org.atnos.eff

import org.specs2.Specification
import org.atnos.eff.syntax.all._
import all._

import scala.concurrent._
import duration._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.data.Xor
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.XorMatchers._

class SafeFutureEffectSpec(implicit ee: ExecutionEnv) extends Specification { def is = s2"""

 A safe future effect can be added to a stack of effects    $e1
 We can attempt a safe future and recover from an exception $e2

"""

  def e1 = {
    def action[R :_safeFuture :_option]: Eff[R, Int] = for {
      a <- safeAsync[R, Int](10)(implicitly[_safeFuture[R]], global)
      b <- option.some(a)
    } yield a + b

    type S = Option |: SafeFuture |: NoEffect

    action[S].runOption.awaitSafeFuture(1.second).run ==== Xor.right(Some(20))
  }

  def e2 = {
    def action[R :_safeFuture]: Eff[R, Throwable Xor Int] =
      Future { throw new TimeoutException; 1 }.safeAttempt[R](implicitly[_safeFuture[R]], global)

    type S = Option |: SafeFuture |: NoEffect

    action[S].map {
      case Xor.Left(e)  => 2
      case Xor.Right(i) => i
    }.runOption.detach.run() must beXorRight(Option(2)).await
  }
}
