package org.atnos.eff.syntax

import org.atnos.eff._
import scala.concurrent._, duration._
import cats.data._

object future extends future

trait future {

  implicit class FutureEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def awaitFuture[U <: Effects](atMost: Duration)
      (implicit member: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable Xor A] =
      FutureInterpretation.awaitFuture(e)(atMost)

  }

  implicit class FutureOps[A](f: Future[A]) {

    def liftFuture[R <: Effects](implicit m: Member[Future, R], e: Member[EvalEffect.Eval, R]) =
      FutureEffect.liftFuture(f)
  }

}
