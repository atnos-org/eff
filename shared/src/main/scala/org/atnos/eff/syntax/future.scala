package org.atnos.eff.syntax

import org.atnos.eff._
import scala.concurrent._, duration._
import cats.data._
import FutureEffect._
import EvalEffect._
import EitherEffect._

object future extends future

trait future {

  implicit class FutureEffectOps[R, A](e: Eff[R, A]) {

    def awaitFuture[U](atMost: Duration)
      (implicit member: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable Either A] =
      FutureInterpretation.awaitFuture(e)(atMost)

  }

  implicit class FutureOps[A](f: =>Future[A]) {

    def liftFuture[R :_future :_eval] =
      FutureEffect.liftFuture(f)

    def attemptFuture[R :_future :_eval :_throwableEither](implicit ec: ExecutionContext) =
      FutureEffect.attemptFuture(f)
  }

}
