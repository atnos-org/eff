package org.atnos.eff

import org.specs2.{ScalaCheck, Specification}
import Eff._
import Effects._
import ReaderEffect._
import OptionEffect._
import org.scalacheck.Gen.posNum

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
        s1 <- OptionEffect.some[S, String]("hello")
        s2 <- OptionEffect.none[S, String]
      } yield s1 + " " + s2

    run(runOption(option)) === None
  }

  def optionReader = prop { (init: Int, someValue: Int) =>

    // define a Reader / Option stack
    type R[A] = Reader[Int, A]
    type S = Option |: R |: NoEffect

    // create actions
    val readOption: Eff[S, Int] =
      for {
        j <- OptionEffect.some[S, Int](someValue)
        i <- ask[S, Int]
      } yield i + j

    // run effects
    run(runReader(init)(runOption(readOption))) must_== Some(init + someValue)
  }.setGens(posNum[Int], posNum[Int])


  def stacksafeOption = {
    type E = Option |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => OptionEffect.some(i))

    run(runOption(action)) ==== Some(list)
  }

}
