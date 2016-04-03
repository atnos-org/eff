package org.atnos.eff

import org.specs2._
import Eff._
import Effects._
import ListEffect._

import cats.syntax.all._
import cats.std.all._

class ListSpec extends Specification { def is = s2"""

 List effect example $listEffect

 The List effect is stack-safe $stackSafe

"""

  type L = List |: NoEffect

  def listEffect = {
    val action: Eff[L, Int] = for {
      a <- singleton(2)
      b <- fromList((1 to a).toList)
      c <- singleton(3)
      d <- fromList((1 to c).toList)
    } yield b + d

    run(runList(action)) ==== List(2, 3, 4, 3, 4, 5)
  }

  def stackSafe = {
    val list = (1 to 5000).toList

    val action: Eff[L, List[Int]] =
      list.traverseU(i => singleton[L, Int](i))

    run(runList(action)).flatten must_== list
  }

}

