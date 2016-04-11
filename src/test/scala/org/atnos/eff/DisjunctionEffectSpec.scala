package org.atnos.eff

import com.ambiata.disorder.{PositiveIntSmall, PositiveLongSmall}
import DisjunctionEffect._
import ReaderEffect._
import Eff._
import Effects._
import org.specs2.{ScalaCheck, Specification}

import cats.data._, Xor._
import cats.syntax.all._
import cats.std.all._

class DisjunctionEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the disjunction monad                     $disjunctionMonad
 run the disjunction monad with nothing        $disjunctionWithKoMonad
 run the disjunction monad with reader         $disjunctionReader

 run is stack safe with Disjunction                          $stacksafeRun
 a left value can be caught and transformed to a right value $leftToRight

"""

  def disjunctionMonad = {
    type S = DisjunctionString |: NoEffect

    val disjunction: Eff[S, Int] =
      for {
        i <- DisjunctionEffect.right[S, String, Int](1)
        j <- DisjunctionEffect.right[S, String, Int](2)
      } yield i + j

    run(runDisjunction(disjunction)) === Right(3)
  }

  def disjunctionWithKoMonad = {
    type S = DisjunctionString |: NoEffect

    val disjunction: Eff[S, Int] =
      for {
        i <- DisjunctionEffect.right[S, String, Int](1)
        j <- DisjunctionEffect.left[S, String, Int]("error!")
      } yield i + j

    run(runDisjunction(disjunction)) === Left("error!")
  }

  def disjunctionReader = prop { (init: PositiveLongSmall, someValue: PositiveIntSmall) =>

    // define a Reader / Disjunction stack
    type ReaderLong[A] = Reader[Long, A]
    type S = DisjunctionString |: ReaderLong |: NoEffect

    // create actions
    val readDisjunction: Eff[S, Int] =
      for {
        j <- DisjunctionEffect.right[S, String, Int](someValue.value)
        i <- ask[S, Long]
      } yield i.toInt + j

    // run effects
    val initial = init.value

    run(runReader(initial)(runDisjunction(readDisjunction))) must_==
      Right(initial.toInt + someValue.value)

  }

  type DisjunctionString[A] = String Xor A

  def stacksafeRun = {
    type E = DisjunctionString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => DisjunctionEffect.right[E, String, String](i.toString))

    run(DisjunctionEffect.runDisjunction(action)) ==== Right(list.map(_.toString))
  }

  def leftToRight = {
    case class TooBig(value: Int)
    type D[A] = TooBig Xor A
    type E = D |: NoEffect

    val i = 7

    val value: Eff[E, Int] =
      if (i > 5) DisjunctionEffect.left[E, TooBig, Int](TooBig(i))
      else       DisjunctionEffect.right[E, TooBig, Int](i)

    val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) { case TooBig(k) =>
      if (k < 10) DisjunctionEffect.right[E, TooBig, Int](k)
      else        DisjunctionEffect.left[E, TooBig, Int](TooBig(k))
    }

    run(runDisjunction(action)) ==== Right(7)
  }
}

