package org.atnos.eff

import cats.syntax.all.*
import org.atnos.eff.all.*
import org.atnos.eff.syntax.all.given
import org.specs2.Specification

class ChooseEffectSpec extends Specification {
  def is = s2"""

 An action can use some non-deterministic choice
   for lists   $nondetList
   for options $nondetOption

 The Choose effect must be stack-safe $stacksafeRun

"""

  type R = Fx.fx2[Choose, Option]

  def nondetList = {

    val action: Eff[R, Int] = for {
      i <- OptionEffect.some[R, Int](1)
      j <- OptionEffect.some[R, Int](2)
      k <- chooseFrom[R, Int](List(i, j))
    } yield k

    action.runChoose[List].runOption.run ==== Some(List(1, 2))
  }

  def nondetOption = {

    val action: Eff[R, Int] = for {
      _ <- OptionEffect.some[R, Int](1)
      j <- OptionEffect.some[R, Int](2)
      k <- chooseFrom[R, Int](List(j))
    } yield k

    action.runChoose[Option].runOption.run ==== Some(Some(2))
  }

  def stacksafeRun = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => ChooseEffect.chooseFrom(List(i)))

    action.runChoose[List].run must not(throwAn[Exception])
  }

}
