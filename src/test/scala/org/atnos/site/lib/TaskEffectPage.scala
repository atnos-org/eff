package org.atnos.site
package lib

import java.util.concurrent.Executors
import org.atnos.eff.syntax.all.*
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

object TaskEffectPage extends UserGuidePage {
  def is = "Task".title ^ s2"""

The `Task` effect is a thin shim on top of Monix's `Task`. This effect is not bundled in core Eff and requires
the `eff-monix` extension to use.

Now, let's create some `Task` effects:${snippet {

      import org.atnos.eff._
      import org.atnos.eff.addon.monix.task._
      import org.atnos.eff.syntax.addon.monix.task._

      import monix.eval.Task

      type R = Fx.fx2[Task, Option]

      val action: Eff[R, Int] =
        for {
          // create a value now
          a <- Eff.pure[R, Int](1)

          // evaluate a value later, on the thread pool specified by a Monix `Scheduler`, and continue when it's finished
          b <- taskDelay[R, Int](1)
        } yield b

      /*p
Then we need to pass a Monix `Scheduler`  in to begin the computation.
       */

      implicit val scheduler: monix.execution.Scheduler =
        monix.execution.Scheduler(ExecutionContext.fromExecutorService(Executors.newScheduledThreadPool(10)): ExecutionContext)

      /*p
Monix doesn't natively offer an Await API to block on a Task result.
Instead it advises converting to a Scala `Future` and using `Await.result`.
See https://monix.io/docs/3x/eval/task.html#blocking-for-a-result
       */
      import scala.concurrent.Await

      Await.result(action.runOption.runAsync.runToFuture, 1.second)
    }.eval}

"""
}
