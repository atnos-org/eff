package org.atnos.example

import org.specs2.Specification
import org.atnos.eff._
import cats.syntax.all._
import cats.data._, Xor._
import ReaderEffect._
import WriterEffect._
import EvalEffect._
import Effects._
import Member.<=
import Eff._

class ReadmeSpec extends Specification { def is = s2"""

 run the first example $firstExample
 future effect $futureEffect

"""

  def firstExample = {

    object StackEffects {
      type ReaderInt[A] = Reader[Int, A]
      type WriterString[A] = Writer[String, A]
      type Stack = ReaderInt |: WriterString |: Eval |: NoEffect
    }

    import StackEffects._

    // create an action
    val action: Eff[Stack, Int] = for {
    // get the configuration
      init <- ask[Stack, Int]

      // log the current configuration value
      _ <- tell[Stack, String]("START: the start value is "+init)

      // compute the nth power of 2
      a <- delay[Stack, Int](powerOfTwo(init))

      // log an end message
      _ <- tell[Stack, String]("END")
    } yield a

    // run the action with all the interpreters
    val result: (Int, List[String]) =
      run(runEval(runWriter(runReader(5)(action))))

    result === ((32, List("START: the start value is 5", "END")))
  }

  import scala.concurrent._, duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import Interpret._

  def futureEffect = {
    object FutureEffect {

      type Fut[A] = Future[() => A]

      def future[R, A](a: =>A)(implicit m: Fut <= R): Eff[R, A] =
        send[Fut, R, A](Future(() => a))

      def runFuture[R <: Effects, A, B](atMost: Duration)(effects: Eff[Fut |: R, A]): Eff[R, A] = {
        val recurse = new Recurse[Fut, R, A] {
          def apply[X](m: Fut[X]): X Xor Eff[R, A] =
            Left(Await.result(m.map(_()), atMost))
        }
        interpret1((a: A) => a)(recurse)(effects)
      }
    }

    import FutureEffect._

    type F = Fut |: NoEffect

    val action: Eff[F, Int] = for {
      a <- future[F, Int](1)
      b <- future[F, Int](2)
    } yield a + b

    run(runFuture(3.seconds)(action)) ==== 3
  }

  def powerOfTwo(n: Int): Int =
    math.pow(2, n.toDouble).toInt
}

