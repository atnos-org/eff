package org.atnos.eff.syntax

import org.atnos.eff._
import scala.concurrent._, duration._
import cats.data._
import SafeFutureEffect._

object safeFuture extends safeFuture

trait safeFuture {

  implicit class SafeFutureEffectOps[R, A](e: Eff[R, A]) {

    def awaitSafeFuture[U](atMost: Duration)
      (implicit member: Member.Aux[SafeFuture, R, U], ec: ExecutionContext): Eff[U, Throwable Xor A] =
      SafeFutureInterpretation.awaitSafeFuture(e)(atMost)

  }

  implicit class SafeFutureOps[A](f: =>Future[A]) {
    def safeLift[R :_safeFuture](implicit ec: ExecutionContext): Eff[R, A] =
      SafeFutureCreation.safeLift(f)

    def safeAttempt[R :_safeFuture](implicit ec: ExecutionContext): Eff[R, Throwable Xor A] =
      SafeFutureCreation.safeAttempt(f)
  }

}
