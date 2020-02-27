package org.atnos.eff.concurrent

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

/**
 * The design of the Scheduler is taken from:
 * [[https://github.com/functional-streams-for-scala/fs2/blob/series/1.0/core/jvm/src/main/scala/fs2/Scheduler.scala]]
 */
trait Scheduler {
  type Cancel = () => Unit

  /**
   * schedule an action which will start after the given duration
   * The return "Cancel" action can allow the action to be cancelled before
   * the duration expires
   */
  def schedule(action: =>Unit, duration: FiniteDuration): Cancel

  /**
   * return a Future which will only complete after the given duration
   */
  def delay(duration: FiniteDuration): Future[Unit]

}
