package org.atnos.site

object ApplicativeEvaluation extends UserGuidePage {
  def is = "Applicative".title ^ s2"""

### Concurrent evaluation

The default interpretation of `Eff` values is "monadic" meaning that effectful values are being evaluated in order. This
  becomes clear when traversing a list of values with the `FutureEffect`:${snippet {
      import org.atnos.eff._, all._, future._, syntax.all.given
      import cats.Eval
      import cats.data.Writer
      import cats.syntax.traverse._
      import scala.concurrent._, duration._, ExecutionContext.Implicits.global
      import org.atnos.eff.concurrent.Scheduler
      import org.atnos.eff.syntax.future.given

      type WriterString[A] = Writer[String, A]
      type _writerString[R] = WriterString |= R

      type S = Fx.fx3[Eval, TimedFuture, WriterString]

      given Scheduler = ExecutorServices.schedulerFromGlobalExecutionContext

      def execute[E: _eval: _writerString: _future](i: Int): Eff[E, Int] =
        for {
          i1 <- delay(i)
          i2 <- futureDelay(i1)
          _ <- tell(i2.toString)
        } yield i2

      val action: Eff[S, List[Int]] =
        List(1000, 500, 50).traverse(execute[S])

      Await.result(action.runEval.runWriterLog.runSequential, 2.seconds)

    }.eval}


We can however run all those computations concurrently using the applicative execution for `Eff`:${snippet {
      // 8<--
      import org.atnos.eff._, all._, future._, syntax.all.given
      import org.atnos.eff.concurrent.Scheduler
      import cats.Eval
      import cats.data.Writer
      import scala.concurrent._, duration._, ExecutionContext.Implicits.global

      type WriterString[A] = Writer[String, A]
      type _writerString[R] = WriterString |= R

      type S = Fx.fx3[Eval, TimedFuture, WriterString]
      given scheduler: Scheduler = ExecutorServices.schedulerFromGlobalExecutionContext

      def execute[E: _eval: _writerString: _future](i: Int): Eff[E, Int] =
        for {
          i1 <- delay(i)
          i2 <- futureDelay(i1)
          _ <- tell(i2.toString)
        } yield i2
// 8<--

      val action: Eff[S, List[Int]] =
        List(1000, 500, 50).traverseA(execute[S])

      Await.result(
        Eff
          .detachA(action.runEval.runWriterLog[String])(using TimedFuture.MonadTimedFuture, TimedFuture.ApplicativeTimedFuture)
          .runNow(scheduler, global),
        2.seconds
      )
    }.eval}

This uses now `traverseA` (instead of `traverse`) to do an applicative traversal and execute futures concurrently and
the fastest actions finish first.

"""

}
