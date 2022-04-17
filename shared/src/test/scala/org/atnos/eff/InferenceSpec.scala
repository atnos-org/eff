package org.atnos.eff

import all._
import syntax.all._
import cats.data._
import org.specs2.Specification

class InferenceSpec extends Specification {
  def is = s2"""

 All the examples should compile ok

"""
  def e1 = {
    import Example1._

    putAndTell[S](4).runState(0).runWriter.runReader("").run
    putAndTell[S](4).runReader("").runState(0).runWriter.run

    // in this case a type annotation is required on runWriter
    putAndTell[S](4).runReader("").runWriter[String].runState(0).run

    ok
  }
}

object Example1 {
  type RNG[R] = Member[State[Int, *], R]
  type Log[R] = Member[Writer[String, *], R]
  type Env[R] = Member[Reader[String, *], R]

  type S = Fx.fx3[State[Int, *], Writer[String, *], Reader[String, *]]

  def putAndTell[R: RNG: Log: Env](i: Int) =
    for {
      _ <- put(i)
      _ <- tell("stored " + i)
    } yield i

}
