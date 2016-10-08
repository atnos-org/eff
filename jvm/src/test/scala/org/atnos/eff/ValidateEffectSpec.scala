package org.atnos.eff

import cats.data.Xor._
import cats.data._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2.{ScalaCheck, Specification}

class ValidateEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the validate effect                     $validateOk
 run the validate effect with nothing        $validateKo
 recover from wrong values                   $catchWrongValues1
 recover from wrong values and tell errors   $catchWrongValues2

 run is stack safe with Validate  $stacksafeRun

"""
  type S = Fx.fx1[ValidateString]

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
        _ <- ValidateEffect.wrong[S, String]("error1")
        _ <- ValidateEffect.wrong[S, String]("error2")
        a <- EffMonad[S].pure(3)
      } yield a

    validate.runNel.run ==== Left(NonEmptyList.of("error1", "error2"))
  }

  def catchWrongValues1 = {
    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.wrong[S, String]("error!")
        a <- EffMonad[S].pure(3)
      } yield a

    validate.catchWrong((s: String) => pure(4)).runNel.run ==== Xor.Right(4)
  }

  def catchWrongValues2 = {
    type E = String
    type Comput = Fx.fx2[Validate[E, ?], Writer[E,?]]
    type Check[A] = Eff[Comput, A]

    def runCheck[A](c: Check[A]) = c.runNel.runWriter.run

    val handle: E => Check[Unit] = { case e => tell[Comput, E](e).as(()) }

    val comp1: Check[Int] = for {
      _ <- wrong[Comput, E]("1").catchWrong(handle)
      _ <- wrong[Comput, E]("2").catchWrong(handle)
    } yield 0

    val comp2: Check[Int] = comp1

    comp2.runNel.runWriter.run ==== ((Right(0), List("1", "2")))
  }

  type ValidateString[A] = Validate[String, A]

  def stacksafeRun = {
    val list = (1 to 5000).toList
    val action = list.traverseU(i => ValidateEffect.wrong[S, String](i.toString))

    action.runNel.run ==== NonEmptyList.fromList(list.map(_.toString)).map(Xor.left).getOrElse(Xor.right(Nil))
  }

}

