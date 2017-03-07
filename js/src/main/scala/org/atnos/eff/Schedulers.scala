package org.atnos.eff

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

    override def toString = "Scheduler"
  }

}

object Schedulers extends Schedulers

