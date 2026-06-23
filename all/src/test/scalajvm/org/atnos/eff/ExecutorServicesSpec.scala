package org.atnos.eff

import org.specs2.Specification
import scala.concurrent.ExecutionContext

class ExecutorServicesSpec(using ec: ExecutionContext) extends Specification {
  def is = s2"""

 Executor services can be created from an execution context $fromExecutionContext

  """

  def fromExecutionContext = {
    val executorServices = ExecutorServices.fromExecutionContext(ec)
    executorServices.executionContext ==== ec
  }
}
