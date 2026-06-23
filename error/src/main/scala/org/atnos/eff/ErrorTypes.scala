package org.atnos.eff

trait ErrorTypes[F] {

  /** type of errors: exceptions or failure messages */
  type Error = Either[Throwable, F]

  /**
   * base type for this effect: either an error or a computation to evaluate
   * a "by-name" value
   */
  type ErrorOrOk[A] = Evaluate[F, A]

  type _ErrorOrOk[R] = ErrorOrOk <= R
  type _errorOrOk[R] = ErrorOrOk |= R
}
