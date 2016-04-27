package org.atnos.eff

import org.scalacheck.Gen.posNum
import org.specs2.{ScalaCheck, Specification}

import cats.data._, Xor._
import cats.syntax.all._
import cats.std.all._
import org.atnos.eff.all._
import org.atnos.eff.implicits._
import org.atnos.eff.syntax.all._

class XorEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the xor effect monad                     $xorMonad
 run the xor effect monad with nothing        $xorWithKoMonad
 run the xor effect monad with reader         $xorReader

 run is stack safe with Xor                   $stacksafeRun
 a left value can be caught and transformed to a right value $leftToRight

"""

  def xorMonad = {
    type S = XorString |: NoEffect

    val xor: Eff[S, Int] =
      for {
        i <- XorEffect.right[S, String, Int](1)
        j <- XorEffect.right[S, String, Int](2)
      } yield i + j

    xor.runXor.run === Right(3)
  }

  def xorWithKoMonad = {
    type S = XorString |: NoEffect

    val xor: Eff[S, Int] =
      for {
        i <- XorEffect.right[S, String, Int](1)
        j <- XorEffect.left[S, String, Int]("error!")
      } yield i + j

    xor.runXor.run === Left("error!")
  }

  def xorReader = prop { (init: Long, someValue: Int) =>

    // define a Reader / Xor stack
    type ReaderLong[A] = Reader[Long, A]
    type S = XorString |: ReaderLong |: NoEffect

    // create actions
    val readXor: Eff[S, Int] =
      for {
        j <- XorEffect.right[S, String, Int](someValue)
        i <- ask[S, Long]
      } yield i.toInt + j

    // run effects
    readXor.runXor.runReader(init).run must_==
      Right(init.toInt + someValue)

  }.setGens(posNum[Long], posNum[Int])

  type XorString[A] = String Xor A

  def stacksafeRun = {
    type E = XorString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => XorEffect.right[E, String, String](i.toString))

    action.runXor.run ==== Right(list.map(_.toString))
  }

  def leftToRight = {
    case class TooBig(value: Int)
    type D[A] = TooBig Xor A
    type E = D |: NoEffect

    val i = 7

    val value: Eff[E, Int] =
      if (i > 5) XorEffect.left[E, TooBig, Int](TooBig(i))
      else       XorEffect.right[E, TooBig, Int](i)

    val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) { case TooBig(k) =>
      if (k < 10) XorEffect.right[E, TooBig, Int](k)
      else        XorEffect.left[E, TooBig, Int](TooBig(k))
    }

    action.runXor.run ==== Right(7)
  }
}

