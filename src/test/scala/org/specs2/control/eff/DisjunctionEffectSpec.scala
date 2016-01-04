package org.specs2.control.eff

import com.ambiata.disorder._
import DisjunctionEffect._
import Eff._
import Effects._
import MemberNat._
import ReaderEffect._
import org.specs2.{ScalaCheck, Specification}

import cats.data._, Xor._
import cats.syntax.all._
import cats.std.all._

class DisjunctionEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the disjunction monad                     $disjunctionMonad
 run the disjunction monad with nothing        $disjunctionWithKoMonad
 run the disjunction monad with reader         $disjunctionReader

 run is stack safe with Disjunction      $stacksafeRun

"""

  def disjunctionMonad = {
    type S = DisjunctionString |: NoEffect

    implicit def DisjunctionStack: Member[DisjunctionString, S] =
      Member.MemberNatIsMember

    val disjunction: Eff[S, Int] =
      for {
        i <- DisjunctionEffect.right[S, String, Int](1)
        j <- DisjunctionEffect.right[S, String, Int](2)
      } yield i + j

    run(runDisjunction(disjunction)) === Right(3)
  }

  def disjunctionWithKoMonad = {
    type S = DisjunctionString |: NoEffect

    implicit def DisjunctionStack: Member[DisjunctionString, S] =
      Member.MemberNatIsMember

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
    type Stack = DisjunctionString |: ReaderLong |: NoEffect

    implicit def ReaderStack: Member[ReaderLong, Stack] =
      Member.MemberNatIsMember

    implicit def DisjunctionStringStack: Member[DisjunctionString, Stack] =
      Member.MemberNatIsMember

    // create actions
    val readDisjunction: Eff[Stack, Int] =
      for {
        j <- DisjunctionEffect.right(someValue.value)
        i <- ask[Stack, Long]
      } yield i.toInt + j

    // run effects
    val initial = init.value

    run(runReader(initial)(runDisjunction(readDisjunction))) must_==
      Right(initial.toInt + someValue.value)
  }

  type DisjunctionString[A] = String Xor A

  def stacksafeRun = {
    type E = DisjunctionString |: NoEffect
    implicit def DisjunctionStringMember: Member[DisjunctionString, E] =
      Member.MemberNatIsMember

    val list = (1 to 5000).toList
    val action = list.traverseU(i => DisjunctionEffect.right(i.toString))

    run(DisjunctionEffect.runDisjunction(action)) ==== Right(list.map(_.toString))
  }

}
