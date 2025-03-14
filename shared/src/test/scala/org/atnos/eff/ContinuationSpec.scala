package org.atnos.eff

import cats.*
import org.atnos.eff.syntax.all.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.specs2.ScalaCheck
import org.specs2.Specification

class ContinuationSpec extends Specification with ScalaCheck with Specs2Compat {
  def is = s2"""

 A function can run at the end of a Kleisli arrow into the Eff monad $mapLast

"""

  def mapLast = prop { (xs: List[Int]) =>
    type R = Fx.fx1[Eval]
    val plusOne = Continuation.unit[R, Int].mapLast(_.map(_ + 1))
    xs.traverseA(plusOne).runEval.run ==== xs.map(_ + 1)
  }.setGen(Gen.listOf(Gen.oneOf(1, 2, 3)))

}
