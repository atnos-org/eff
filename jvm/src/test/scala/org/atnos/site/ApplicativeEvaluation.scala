package org.atnos.site

object ApplicativeEvaluation extends UserGuidePage { def is = "Applicative".title ^ s2"""

The default interpretation of `Eff` values is "monadic" meaning that effectful values are being evaluated in order. This
  becomes clear when traversing a list of values with the `FutureEffect`:${snippet{
import org.atnos.eff._, all._, syntax.all._
import cats.Eval
import cats.data.Writer
import cats.syntax.traverse._
import cats.std.list._
import scala.concurrent._, duration._, ExecutionContext.Implicits.global

type WriterString[A] = Writer[String, A]
type _writerString[R] = WriterString |= R

type S = Fx.fx3[Eval, Future, WriterString]

def execute[E :_eval :_writerString :_future](i: Int): Eff[E, Int] =
  for {
    i1 <- delay(i)
    i2 <- async(i1)
    _  <- tell(i2.toString)
  } yield i2

val action: Eff[S, List[Int]] =
  List(1000, 500, 50).traverse(execute[S])

action.runEval.awaitFuture(2.seconds).runWriterLog.run

}.eval}


We can however run all those computations concurrently using the applicative execution for `Eff`:${snippet{
 // 8<--
import org.atnos.eff._, all._, syntax.all._
import cats.Eval
import cats.data.Writer
import cats.std.list._
import scala.concurrent._, duration._, ExecutionContext.Implicits.global

type WriterString[A] = Writer[String, A]
type _writerString[R] = WriterString |= R

type S = Fx.fx3[Eval, Future, WriterString]

def execute[E :_eval :_writerString :_future](i: Int): Eff[E, Int] =
  for {
    i1 <- delay(i)
    i2 <- async(i1)
    _ <- tell(i2.toString)
  } yield i2
// 8<--

val action: Eff[S, List[Int]] =
  List(1000, 500, 50).traverseA(execute[S])

action.runEval.awaitFuture(2.seconds).runWriterLog.run
}.eval}

This uses now `traverseA` (instead of `traverse`) to do an applicative traversal and execute futures concurrently and
the fastest actions finish first.

"""
}

