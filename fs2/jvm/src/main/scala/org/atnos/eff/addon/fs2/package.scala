package org.atnos.eff.addon

import org.atnos.eff.ExecutorServices
import _root_.fs2.{Scheduler, Strategy}

package object fs2 extends TaskEffect {
  /** implement in js and jvm */
  @deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
  def fromExecutorServices(es: ExecutorServices): AsyncTasks = {
    val s: Strategy = Strategy.fromExecutionContext(es.executionContext)
    val sc: Scheduler = Scheduler.fromScheduledExecutorService(es.scheduledExecutorService)
    AsyncTasks()(s, sc, es)
  }
}
