package org.atnos.eff.addon

import org.atnos.eff.ExecutorServices
import _root_.fs2.{Scheduler, Strategy}

package object fs2 extends AsyncTaskInterpreter {
  /** implement in js and jvm */
  def fromExecutorServices(es: ExecutorServices): AsyncTasks = {
    val s: Strategy = Strategy.fromExecutionContext(es.executionContext)
    val sc: Scheduler = Scheduler.fromScheduledExecutorService(es.scheduledExecutorService)
    AsyncTasks()(s, sc, es)
  }
}
