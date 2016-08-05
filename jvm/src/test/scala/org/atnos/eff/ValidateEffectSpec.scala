package org.atnos.eff

import cats.data.Xor._
import cats.data._
import cats.std.all._
import cats.syntax.all._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2.{ScalaCheck, Specification}

class ValidateEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the validate effect                     $validateOk
 run the validate effect with nothing        $validateKo
 recover from wrong values                   $catchWrongValues

 run is stack safe with Validate  $stacksafeRun

"""
  type S = ValidateString |: NoEffect

  def validateOk = {

    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.correct[S, String, Int](2)
        a <- EffMonad[S].pure(3)
      } yield a

    validate.runNel.run ==== Right(3)
  }

  def validateKo = {
    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.wrong[S, String]("error!")
        a <- EffMonad[S].pure(3)
      } yield a

    validate.runNel.run ==== Left(NonEmptyList("error!"))
  }

  def catchWrongValues = {
    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.wrong[S, String]("error!")
        a <- EffMonad[S].pure(3)
      } yield a

    validate.catchWrong((s: String) => pure(4)).runNel.run ==== Xor.Right(4)
  }

  type ValidateString[A] = Validate[String, A]

  def stacksafeRun = {
    val list = (1 to 5000).toList
    val action = list.traverseU(i => ValidateEffect.wrong[S, String](i.toString))

    action.runNel.run ==== NonEmptyList.fromList(list.map(_.toString)).map(Xor.left).getOrElse(Xor.right(Nil))
  }

}

