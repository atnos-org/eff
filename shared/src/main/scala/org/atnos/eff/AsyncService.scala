package org.atnos.eff

import org.atnos.eff.async._

import scala.concurrent.duration.FiniteDuration

trait AsyncService {

  /** synchronous computation */
  def asyncNow[R :_async, A](a: A): Eff[R, A]

  /** failed computation */
  def asyncFail[R :_async](t: Throwable): Eff[R, Unit]

  /** computation which will be evaluated later */
  def asyncDelay[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A]

  /** asynchronous computation evaluated on a different thread */
  def asyncFork[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A]

  /** evaluate an Async computation on another thread */
  def fork[R :_async, A](a: =>Async[A], timeout: Option[FiniteDuration] = None): Eff[R, A]

  /** register a callback to create an asynchronous computation */
  def async[R :_async, A](register: ((Throwable Either A) => Unit) => Unit, timeout: Option[FiniteDuration] = None): Eff[R, A]
}

