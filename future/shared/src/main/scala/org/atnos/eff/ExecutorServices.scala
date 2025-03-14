package org.atnos.eff

import cats.Eval
import java.util.Collections
import java.util.concurrent.*
import org.atnos.eff.concurrent.Scheduler
import org.atnos.eff.concurrent.Schedulers
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.Promise
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

case class ExecutorServices(
  executorServiceEval: Eval[ExecutorService],
  scheduledExecutorEval: Eval[ScheduledExecutorService],
  executionContextEval: Eval[ExecutionContext]
) {

  /** note: shutdown only shuts down the executor services */
  def shutdown: Eval[Unit] = Eval.later {
    // careful: calling executorService.shutdown or scheduledExecutorService will deadlock!
    try executorServiceEval.value.shutdown()
    finally scheduledExecutorEval.value.shutdown()
  }

  implicit lazy val executorService: ExecutorService =
    executorServiceEval.value

  implicit lazy val scheduledExecutorService: ScheduledExecutorService =
    scheduledExecutorEval.value

  implicit lazy val executionContext: ExecutionContext =
    executionContextEval.value

  implicit lazy val scheduler: Scheduler =
    ExecutorServices.schedulerFromScheduledExecutorService(scheduledExecutorService)

  /** convenience method to shutdown the services when the final future has completed */
  def shutdownOnComplete[A](future: scala.concurrent.Future[A]): ExecutorServices = {
    future.onComplete(_ => shutdown.value)
    this
  }

}

object ExecutorServices extends Schedulers {

  lazy val threadsNb = Runtime.getRuntime.availableProcessors

  def create(implicit es: ExecutorService, s: ScheduledExecutorService): ExecutorServices =
    fromExecutorServices(es, s)

  def fromExecutorServices(es: => ExecutorService, s: => ScheduledExecutorService): ExecutorServices =
    ExecutorServices(
      Eval.later(es),
      Eval.later(s),
      Eval.later(createExecutionContext(es))
    )

  def fromExecutorService(es: => ExecutorService): ExecutorServices =
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
  def fromExecutionContext(ec: => ExecutionContext): ExecutorServices =
    ExecutorServices(Eval.later(executorFromExecutionContext(ec)), Eval.later(scheduledExecutor(threadsNb)), Eval.later(ec))

  /** taken from [[https://gist.github.com/viktorklang/5245161]] */
  def executorFromExecutionContext(ec: => ExecutionContext): ExecutorService =
    ec match {
      case null => throw null
      case eces: ExecutionContextExecutorService => eces
      case other =>
        new AbstractExecutorService with ExecutionContextExecutorService {
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

  /** create an ExecutorServices from Scala global execution context */
  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

  /** create a Scheduler from Scala global execution context */
  def schedulerFromGlobalExecutionContext: Scheduler =
    schedulerFromScheduledExecutorService(fromGlobalExecutionContext.scheduledExecutorService)

  def schedulerFromScheduledExecutorService(s: ScheduledExecutorService): Scheduler =
    new Scheduler {
      def schedule(timedout: => Unit, duration: FiniteDuration): () => Unit = {
        val scheduled = s.schedule(new Runnable { def run(): Unit = timedout }, duration.toNanos, TimeUnit.NANOSECONDS)
        () => { scheduled.cancel(false); () }
      }

      def delay(duration: FiniteDuration): scala.concurrent.Future[Unit] = {
        val p = Promise[Unit]()
        s.schedule(new Runnable { def run(): Unit = { p.complete(Try(())); () } }, duration.toNanos, TimeUnit.NANOSECONDS)
        p.future
      }

      override def toString = "Scheduler"
    }

}
