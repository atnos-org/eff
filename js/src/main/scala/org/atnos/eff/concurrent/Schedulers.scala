package org.atnos.eff.concurrent

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js.timers._

trait Schedulers {

  /**
   * Default Scheduler for JavaScript
   */
  def default: Scheduler = new Scheduler {
    def schedule(timedout: =>Unit, duration: FiniteDuration): () => Unit = {
      val handle = setTimeout(duration)(timedout)
      () => clearTimeout(handle)
    }

    def delay(duration: FiniteDuration): Future[Unit] =
      sys.error("delay(duration) not implemented")

    override def toString = "Scheduler"
  }

}

object Schedulers extends Schedulers

