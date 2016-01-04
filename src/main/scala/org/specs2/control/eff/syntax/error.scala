package org.specs2.control.eff
package syntax

import Member.<=
import ErrorEffect._
import cats.data._, Xor._

object error {

  implicit class ErrorEffectOps[R <: Effects, A](action: Eff[R, A]) {
    def andFinally(last: Eff[R, Unit])(implicit m: ErrorOrOk <= R): Eff[R, A] =
      ErrorEffect.andFinally(action, last)

    def orElse(action2: Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
      ErrorEffect.orElse(action, action2)
  }

  implicit class ErrorOrOkOps[A](c: Error Xor A) {
    def toErrorSimpleMessage: Option[String] =
      c match {
        case Left(e) => Some(e.simpleMessage)
        case _      => None
      }

    def toErrorFullMessage: Option[String] =
      c match {
        case Left(e) => Some(e.fullMessage)
        case _      => None
      }
  }

  implicit class ErrorOps[A](e: Error) {
    def simpleMessage: String =
      e match {
        case Left(t) => render(t)
        case Right(m) => m
      }

    def fullMessage: String =
      e match {
        case Left(t) => renderWithStack(t)
        case Right(m) => m
      }
  }


}
