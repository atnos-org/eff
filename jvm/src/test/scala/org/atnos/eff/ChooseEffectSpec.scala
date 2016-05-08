package org.atnos.eff

import org.specs2.Specification
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class ChooseEffectSpec extends Specification { def is = s2"""

 An action can be use non-determinism
   for lists   $nondetList
   for options $nondetOption

"""

  type R = Choose |: Option |: NoEffect

  def nondetList = {

    val action: Eff[R, Int] = for {
      i <- OptionEffect.some[R, Int](1)
      j <- OptionEffect.some[R, Int](2)
      k <- chooseFrom[R, Int](List(i, j))
    } yield k

    import cats.std.list._

    action.runChoose.runOption.run ==== Some(List(1, 2))
  }

  def nondetOption = {

    val action: Eff[R, Int] = for {
      _ <- OptionEffect.some[R, Int](1)
      j <- OptionEffect.some[R, Int](2)
      k <- chooseFrom[R, Int](List(j))
    } yield k

    import cats.std.option._

    action.runChoose.runOption.run ==== Some(Some(2))
  }


}

