package org.atnos.eff

import java.util.Collections
import java.util.concurrent._

import cats.Eval

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

case class ExecutorServices(executorServiceEval:   Eval[ExecutorService],
                            scheduledExecutorEval: Eval[ScheduledExecutorService],
                            executionContextEval:  Eval[ExecutionContext]) {

  /** note: shutdown only shuts down the executor services */
  def shutdown: Eval[Unit] = Eval.later {
    // careful: calling executorService.shutdown or scheduledExecutorService will deadlock!
    try     executorServiceEval.value.shutdown
    finally scheduledExecutorEval.value.shutdown
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

  def fromExecutorService(es: =>ExecutorService): ExecutorServices =
    fromExecutorServices(es, scheduledExecutor(threadsNb))

  def createExecutionContext(executorService: ExecutorService, logger: String => Unit = println): ExecutionContext =
    ExecutionContext.fromExecutorService(executorService, (t: Throwable) => logger(t.getStackTrace.mkString("\n")))

  def executor(threadsNb: Int): ExecutorService =
    Executors.newFixedThreadPool(threadsNb)

  def scheduledExecutor(scheduledThreadsNb: Int): ScheduledExecutorService =
    Executors.newScheduledThreadPool(scheduledThreadsNb)


  /**
   * create an ExecutionEnv from an execution context only
   *
   * WARNING!!! This method create a brand new scheduledExecutorService which will be used if
   * you use the ExecutorServices to timeout an Async effect
   */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutorServices =
     ExecutorServices(
      Eval.later(executorFromExecutionContext(ec)),
      Eval.later(scheduledExecutor(threadsNb)),
      Eval.later(ec))

  /** taken from https://gist.github.com/viktorklang/5245161 */
  def executorFromExecutionContext(ec: =>ExecutionContext): ExecutorService =
    ec match {
      case null => throw null
      case eces: ExecutionContextExecutorService => eces
      case other => new AbstractExecutorService with ExecutionContextExecutorService {
        override def prepare(): ExecutionContext = other
        override def isShutdown = false
        override def isTerminated = false
        override def shutdown() = ()
        override def shutdownNow() = Collections.emptyList[Runnable]
        override def execute(runnable: Runnable): Unit = other execute runnable
        override def reportFailure(t: Throwable): Unit = other reportFailure t
        override def awaitTermination(length: Long, unit: TimeUnit): Boolean = false
      }
    }

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}

