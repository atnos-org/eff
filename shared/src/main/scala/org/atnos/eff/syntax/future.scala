package org.atnos.eff.syntax

import org.atnos.eff._
import scala.concurrent._, duration._
import cats.data._

object future extends future

trait future {

  implicit class FutureEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def awaitFuture[U <: Effects](atMost: FiniteDuration)
      (implicit member: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable Xor A] =
      FutureInterpretation.awaitFuture(e)(atMost)

  }

}
