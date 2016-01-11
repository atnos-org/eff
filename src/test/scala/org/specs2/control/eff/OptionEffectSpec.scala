package org.specs2.control.eff

import cats.Eval
import com.ambiata.disorder._
import org.specs2.{ScalaCheck, Specification}
import Eff._
import Effects._
import ReaderEffect._
import OptionEffect._

import cats.syntax.all._
import cats.std.all._
import cats.data._

class OptionEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the option monad                     $optionMonad
 run the option monad with nothing        $optionWithNothingMonad
 run the option monad with reader         $optionReader

 The Eff monad is stack safe with Option  $stacksafeOption

"""

  def optionMonad = {
    type S = Option |: NoEffect

    val option: Eff[S, String] =
      for {
        s1 <- OptionEffect.some("hello")
        s2 <- OptionEffect.some("world")
      } yield s1 + " " + s2

    run(runOption(option)) === Some("hello world")
  }

  def optionWithNothingMonad = {
    type S = Option |: NoEffect

    val option: Eff[S, String] =
      for {
        s1 <- OptionEffect.some("hello")
        s2 <- OptionEffect.none
      } yield s1 + " " + s2

    run(runOption(option)) === None
  }

  def optionReader = prop { (init: PositiveIntSmall, someValue: PositiveIntSmall) =>

    // define a Reader / Option stack
    type R[A] = Reader[Int, A]
    type S = Option |: R |: NoEffect

    // create actions
    val readOption: Eff[S, Int] =
      for {
        j <- OptionEffect.some[S, Int](someValue.value)
        i <- ask[S, Int]
      } yield i + j

    // run effects
    val initial = init.value

    run(runReader(initial)(runOption(readOption))) must_==
      Some(initial + someValue.value)
  }


  def stacksafeOption = {
    type E = Option |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => OptionEffect.some(i))

    run(runOption(action)) ==== Some(list)
  }

}
