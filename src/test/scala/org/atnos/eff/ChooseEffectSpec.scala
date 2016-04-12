package org.atnos.eff

import Eff._
import Effects._
import ChooseEffect._
import OptionEffect._
import org.specs2.Specification
import cats.syntax.functor._
import cats.syntax.flatMap._


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
      k <- choose[R, Int](List(i, j))
    } yield k

    import cats.std.list._

    run(runOption(runChoose(action))) ==== Some(List(1, 2))
  }

  def nondetOption = {

    val action: Eff[R, Int] = for {
      _ <- OptionEffect.some[R, Int](1)
      j <- OptionEffect.some[R, Int](2)
      k <- choose[R, Int](List(j))
    } yield k

    import cats.std.option._

    run(runOption(runChoose(action))) ==== Some(Some(2))
  }


}

