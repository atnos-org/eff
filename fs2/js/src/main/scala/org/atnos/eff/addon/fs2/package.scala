package org.atnos.eff.addon
import _root_.fs2.{Scheduler, Strategy}
import org.atnos.eff.ExecutorServices

package object fs2  extends AsyncTaskInterpreter {
  /** implement in js and jvm */
  def fromExecutorServices(es: ExecutorServices): AsyncTasks = {
    val s: Strategy = Strategy.fromExecutionContext(es.executionContext)
    val sc: Scheduler = Scheduler.default
    AsyncTasks()(s, sc, es)
  }
}
