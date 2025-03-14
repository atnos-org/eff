package org.atnos.eff

import cats.*
import org.atnos.eff.Eff.*

trait ErrorCreation[F] extends ErrorTypes[F] {

  /** create an Eff value from a computation */
  def ok[R: _errorOrOk, A](a: => A): Eff[R, A] =
    send[ErrorOrOk, R, A](Evaluate.ok[F, A](a))

  /** create an Eff value from a computation */
  def eval[R: _errorOrOk, A](a: Eval[A]): Eff[R, A] =
    send[ErrorOrOk, R, A](Evaluate.eval[F, A](a))

  /** create an Eff value from an error */
  def error[R: _errorOrOk, A](error: Error): Eff[R, A] =
    send[ErrorOrOk, R, A](Evaluate.error[F, A](error))

  /** create an Eff value from a failure */
  def fail[R: _errorOrOk, A](failure: F): Eff[R, A] =
    error(Right(failure))

  /** create an Eff value from an exception */
  def exception[R: _errorOrOk, A](t: Throwable): Eff[R, A] =
    error(Left(t))
}
