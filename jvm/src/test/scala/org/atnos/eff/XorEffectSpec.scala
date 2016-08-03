package org.atnos.eff

import org.scalacheck.Gen.posNum
import org.specs2.{ScalaCheck, Specification}

import cats.data._, Xor._
import cats.syntax.all._
import cats.std.all._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.scalacheck.Gen

class XorEffectSpec extends Specification with ScalaCheck { def is = s2"""

 an xor value can be injected in the stack    $xorCreation

 run the xor effect monad                     $xorMonad
 run the xor effect monad with nothing        $xorWithKoMonad
 run the xor effect monad with reader         $xorReader

 run is stack safe with Xor                   $stacksafeRun
 a left value can be caught and transformed to a right value $leftToRight

 the left type can be modified with local in a different stack $local
 the left type can be run with local in the same stack  $localRun

"""

  def xorCreation = prop { stringOrInt: String Either Int =>
    type S = XorString |: NoEffect

    val xor = Xor.fromEither(stringOrInt)
    val e: Eff[S, Int] = fromXor(xor)
    e.runEither.run ==== xor.toEither
  }

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
    readXor.fx.runXor.runReader(init).run must_==
      Right(init.toInt + someValue)

  }.setGens(posNum[Long], posNum[Int])

  type XorString[A] = String Xor A

  def stacksafeRun = {
    type E = XorString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => XorEffect.right[E, String, String](i.toString))

    action.runXor.run ==== Right(list.map(_.toString))
  }

  def leftToRight = prop { i: Int =>
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

    val expected: TooBig Xor Int =
      if (i < 10) Xor.right(i) else Xor.left(TooBig(i))

    val actual: TooBig Xor Int =
      action.runXor.run

    actual == expected

  }.setGen(Gen.oneOf(14, 12))

  def local = {
    case class Error1(m: String)

    case class Error2(e1: Error1)

    type R1 = (Error1 Xor ?) |: NoEffect
    type R2 = (Error2 Xor ?) |: NoEffect

    val action1: Eff[R1, Unit] =
      XorEffect.left(Error1("boom"))

    val action2: Eff[R2, Unit] =
      action1.localXor(Error2)

    action2.runXor.run ==== Xor.left(Error2(Error1("boom")))
  }

  def localRun = {
    case class Error1(m: String)

    case class Error2(e1: Error1)

    type R1 = (Error1 Xor ?) |: (Error2 Xor ?) |: NoEffect

    val action1: Eff[R1, Unit] =
      XorEffect.left(Error1("boom"))

    action1.runLocalXor(Error2).runXor.run ==== Xor.left(Error2(Error1("boom")))
  }
}

