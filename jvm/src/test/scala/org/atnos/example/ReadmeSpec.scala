package org.atnos.example

import org.specs2.Specification
import org.atnos.eff._
import cats.syntax.all._
import cats.data._, Xor._
import cats.Eval
import ReaderEffect._
import WriterEffect._
import EvalEffect._
import Effects._
import Member.<=
import Eff._

class ReadmeSpec extends Specification { def is = s2"""

 run the first example $firstExample

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

  def powerOfTwo(n: Int): Int =
    math.pow(2, n.toDouble).toInt
}
