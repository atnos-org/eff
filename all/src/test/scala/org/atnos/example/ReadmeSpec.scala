package org.atnos.example

import cats.Eval
import cats.data.*
import org.atnos.eff.*
import org.atnos.eff.Eff.run
import org.atnos.eff.EvalEffect.*
import org.atnos.eff.ReaderEffect.*
import org.atnos.eff.WriterEffect.*
import org.specs2.Specification

class ReadmeSpec extends Specification {
  def is = s2"""

 run the first example $firstExample

"""

  def firstExample = {

    object StackEffects {
      type ReaderInt[A] = Reader[Int, A]
      type WriterString[A] = Writer[String, A]
      type Stack = Fx.fx3[ReaderInt, WriterString, Eval]
    }

    import StackEffects._

    // create an action
    val action: Eff[Stack, Int] = for {
      // get the configuration
      init <- ask[Stack, Int]

      // log the current configuration value
      _ <- tell[Stack, String]("START: the start value is " + init)

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
