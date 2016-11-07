package org.atnos.eff

import org.atnos.eff.async._

import scala.concurrent.duration.FiniteDuration

trait AsyncService {

  def asyncNow[R :_async, A](a: A): Eff[R, A]

  def asyncFail[R :_async](t: Throwable): Eff[R, Unit]

  def asyncDelay[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A]

  def asyncFork[R :_async, A](a: =>A, timeout: Option[FiniteDuration] = None): Eff[R, A]

  def fork[R :_async, A](a: =>Async[A], timeout: Option[FiniteDuration] = None): Eff[R, A]

}

