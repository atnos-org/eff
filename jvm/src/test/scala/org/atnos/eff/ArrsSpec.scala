package org.atnos.eff

import cats._
import cats.instances.all._
import org.atnos.eff.syntax.all._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import org.specs2.{ScalaCheck, Specification}

class ArrsSpec extends Specification with ScalaCheck { def is = s2"""

 A function can run at the end of a Kleisli arrow into the Eff monad $mapLast

  """

  def mapLast = prop { xs: List[Int] =>
    type R = Fx.fx1[Eval]
    val plusOne = Arrs.unit[R, Int].mapLast(_.map(_ + 1))
    xs.traverseA(plusOne).detach.value ==== xs.map(_ + 1)
  }.setGen(Gen.listOf(Gen.oneOf(1, 2, 3)))

  }
