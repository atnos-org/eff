package org.atnos.eff

import java.util.concurrent.{ExecutorService, Executors, ScheduledExecutorService}

import cats.Eval

import scala.concurrent.ExecutionContext

case class ExecutorServices(executorServiceEval:   Eval[ExecutorService],
                            scheduledExecutorEval: Eval[ScheduledExecutorService],
                            executionContextEval:  Eval[ExecutionContext]) {

  /** note: shutdown only shuts down the executor services */
  def shutdown: Eval[Unit] = Eval.later {
    try     { executorService.shutdownNow; () }
    finally { scheduledExecutorService.shutdownNow; () }
  }

  implicit lazy val executorService: ExecutorService =
    executorServiceEval.value

  implicit lazy val scheduledExecutorService: ScheduledExecutorService =
    scheduledExecutorEval.value

  implicit lazy val executionContext: ExecutionContext =
    executionContextEval.value

}

object ExecutorServices {

  lazy val threadsNb = Runtime.getRuntime.availableProcessors

  def create(implicit es: ExecutorService, s: ScheduledExecutorService): ExecutorServices =
    fromExecutorServices(es, s)

  def fromExecutorServices(es: =>ExecutorService, s: =>ScheduledExecutorService): ExecutorServices =
    ExecutorServices(
      Eval.later(es),
      Eval.later(s),
      Eval.later(createExecutionContext(es))
    )

  def createExecutionContext(executorService: ExecutorService, logger: String => Unit = println): ExecutionContext =
    ExecutionContext.fromExecutorService(executorService, (t: Throwable) => logger(t.getStackTrace.mkString("\n")))

  def executor(threadsNb: Int): ExecutorService =
    Executors.newFixedThreadPool(threadsNb)

  def scheduledExecutor(scheduledThreadsNb: Int): ScheduledExecutorService =
    Executors.newScheduledThreadPool(scheduledThreadsNb)


  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutorServices =
    fromExecutorServices(
      executor(threadsNb),
      scheduledExecutor(threadsNb))

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}

