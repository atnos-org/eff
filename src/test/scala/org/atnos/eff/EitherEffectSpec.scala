package org.atnos.eff

import org.scalacheck.Gen.posNum
import org.specs2.{ScalaCheck, Specification}

import cats.data._, Xor._
import cats.syntax.all._
import cats.std.all._
import org.atnos.eff.all._
import org.atnos.eff.implicits._
import org.atnos.eff.syntax.all._

class EitherEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the either effect monad                     $eitherMonad
 run the either effect monad with nothing        $eitherWithKoMonad
 run the either effect monad with reader         $eitherReader

 run is stack safe with Either                   $stacksafeRun
 a left value can be caught and transformed to a right value $leftToRight

"""

  def eitherMonad = {
    type S = EitherString |: NoEffect

    val either: Eff[S, Int] =
      for {
        i <- EitherEffect.right[S, String, Int](1)
        j <- EitherEffect.right[S, String, Int](2)
      } yield i + j

    either.runXor.run === Right(3)
  }

  def eitherWithKoMonad = {
    type S = EitherString |: NoEffect

    val either: Eff[S, Int] =
      for {
        i <- EitherEffect.right[S, String, Int](1)
        j <- EitherEffect.left[S, String, Int]("error!")
      } yield i + j

    either.runXor.run === Left("error!")
  }

  def eitherReader = prop { (init: Long, someValue: Int) =>

    // define a Reader / Either stack
    type ReaderLong[A] = Reader[Long, A]
    type S = EitherString |: ReaderLong |: NoEffect

    // create actions
    val readEither: Eff[S, Int] =
      for {
        j <- EitherEffect.right[S, String, Int](someValue)
        i <- ask[S, Long]
      } yield i.toInt + j

    // run effects
    readEither.runXor.runReader(init).run must_==
      Right(init.toInt + someValue)

  }.setGens(posNum[Long], posNum[Int])

  type EitherString[A] = String Xor A

  def stacksafeRun = {
    type E = EitherString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => EitherEffect.right[E, String, String](i.toString))

    action.runXor.run ==== Right(list.map(_.toString))
  }

  def leftToRight = {
    case class TooBig(value: Int)
    type D[A] = TooBig Xor A
    type E = D |: NoEffect

    val i = 7

    val value: Eff[E, Int] =
      if (i > 5) EitherEffect.left[E, TooBig, Int](TooBig(i))
      else       EitherEffect.right[E, TooBig, Int](i)

    val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) { case TooBig(k) =>
      if (k < 10) EitherEffect.right[E, TooBig, Int](k)
      else        EitherEffect.left[E, TooBig, Int](TooBig(k))
    }

    action.runXor.run ==== Right(7)
  }
}

