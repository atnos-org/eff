package org.atnos.site

object ApplicativeEvaluation extends UserGuidePage { def is = "Applicative".title ^ s2"""

The default interpretation of `Eff` values is "monadic" meaning that effectful values are being evaluated in order. This
  becomes clear when traversing a list of values with the `FutureEffect`:${snippet{
import org.atnos.eff._, all._
import org.atnos.eff.syntax.eval._
import org.atnos.eff.syntax.future._
import org.atnos.eff.syntax.writer._
import org.atnos.eff.syntax.eff._
import cats.Eval
import cats.data.Writer
import cats.syntax.traverse._
import cats.std.list._
import scala.concurrent._, duration._, ExecutionContext.Implicits.global

type Future_[X] = Member[Future, X]
type Eval_[X] = Member[Eval, X]
type Writer_[X] = Member[Writer[String, ?], X]
type S = Eval |: Future |: Writer[String, ?] |: NoEffect

def execute[E : Eval_ : Writer_ : Future_](i: Int): Eff[E, Int] =
  for {
    i1 <- delay(i)
    i2 <- async(i1)
    _  <- tell(i2.toString)
  } yield i2

val action: Eff[S, List[Int]] = List(1000, 500, 50).traverse(execute[S])
action.runEval.awaitFuture(2.seconds).runWriterLog.run

}.eval}


We can however run all those computations concurrently using the applicative execution for `Eff`:${snippet{
import org.atnos.eff._, all._
import org.atnos.eff.syntax.eval._
import org.atnos.eff.syntax.future._
import org.atnos.eff.syntax.writer._
import org.atnos.eff.syntax.eff._
import cats.Eval
import cats.data.Writer
import cats.std.list._
import scala.concurrent._, duration._, ExecutionContext.Implicits.global

type Future_[X] = Member[Future, X]
type Eval_[X] = Member[Eval, X]
type Writer_[X] = Member[Writer[String, ?], X]
type S = Eval |: Future |: Writer[String, ?] |: NoEffect

def execute[E: Eval_ : Writer_ : Future_](i: Int): Eff[E, Int] =
  for {
    i1 <- delay(i)
    i2 <- async(i1)
    _ <- tell(i2.toString)
  } yield i2

val action: Eff[S, List[Int]] = Eff.traverse(List(1000, 500, 50))(execute[S])
action.runEval.awaitFuture(2.seconds).runWriterLog.run
}.eval}
"""
}

