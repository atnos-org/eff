package org.atnos.eff

import org.specs2._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

import cats.syntax.all._
import cats.instances.all._

class ListEffectSpec extends Specification { def is = s2"""

 List effect example       $listEffect
 Empty list effect example $emptyList

 The List effect is stack-safe $stackSafe

"""

  type L = Fx.fx1[List]

  def listEffect = {
    def action[R :_list]: Eff[R, Int] = for {
      a <- singleton(2)
      b <- fromList((1 to a).toList)
      c <- singleton(3)
      d <- fromList((1 to c).toList)
    } yield b + d

    action[L].runList.run ==== List(2, 3, 4, 3, 4, 5)
  }

  def emptyList = {
    def action[R :_list]: Eff[R, Int] =
      (fromList((1 to 0).toList) |@| fromList((1 to 0).toList)).map(_ + _)

    action[L].runList.run ==== List()
  }

  def stackSafe = {
    val list = (1 to 5000).toList

    val action: Eff[L, List[Int]] =
      list.traverse(i => singleton[L, Int](i))

    action.runList.run.flatten must_== list
  }

}

