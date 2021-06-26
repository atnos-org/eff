package org.atnos.eff

import cats._
import org.atnos.eff.syntax.all._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import org.specs2.{ScalaCheck, Specification}

class ContinuationSpec extends Specification with ScalaCheck with Specs2Compat { def is = s2"""

 A function can run at the end of a Kleisli arrow into the Eff monad $mapLast

"""

  def mapLast = prop { (xs: List[Int]) =>
    type R = Fx.fx1[Eval]
    val plusOne = Continuation.unit[R, Int].mapLast(_.map(_ + 1))
    xs.traverseA(plusOne).runEval.run ==== xs.map(_ + 1)
  }.setGen(Gen.listOf(Gen.oneOf(1, 2, 3)))

}
