package org.atnos.eff
package syntax

import scala.language.implicitConversions

import ErrorEffect._
import scala.reflect.ClassTag

object error extends error

trait error {
  implicit final def toErrorEffectOps[R, A](action: Eff[R, A]): ErrorEffectOps[R, A] = new ErrorEffectOps(action)
  implicit final def toErrorOrOkOps[A](c: Error Either A): ErrorOrOkOps[A] = new ErrorOrOkOps(c)
  implicit final def toErrorOps(e: Error): ErrorOps = new ErrorOps(e)
}

final class ErrorEffectOps[R, A](private val action: Eff[R, A]) extends AnyVal {

  def runError(implicit m: Member[ErrorOrOk, R]): Eff[m.Out, Error Either A] =
    ErrorEffect.runError(action)(m.aux)

  def andFinally(last: Eff[R, Unit])(implicit m: ErrorOrOk <= R): Eff[R, A] =
    ErrorEffect.andFinally(action, last)

  def orElse(action2: Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
    ErrorEffect.orElse(action, action2)

  def ignore[E <: Throwable : ClassTag](implicit m: ErrorOrOk <= R): Eff[R, Unit] =
    ErrorEffect.ignoreException(action)
}

final class ErrorOrOkOps[A](private val c: Error Either A) extends AnyVal {
  def toErrorSimpleMessage: Option[String] =
    c match {
      case Left(e) => Some(new ErrorOps(e).simpleMessage)
      case _ => None
    }

  def toErrorFullMessage: Option[String] =
    c match {
      case Left(e) => Some(new ErrorOps(e).fullMessage)
      case _ => None
    }
}

final class ErrorOps(private val e: Error) extends AnyVal {
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


