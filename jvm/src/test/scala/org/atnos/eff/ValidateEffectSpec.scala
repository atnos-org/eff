package org.atnos.eff

import cats.data._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2.{ScalaCheck, Specification}

class ValidateEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the validate effect                     $validateOk
 run the validate effect with nothing        $validateKo

 `Ior`ish or warnings-oriented validation
   run resulting IorNel                      $validateIorOk
   run with warnings                         $validateWarn
   run with warnings and errors              $validateWarnAndErr
   run with errors and warning               $validateWarnAndErr

 recover from wrong values                   $catchWrongValues1
 recover from wrong values and tell errors   $catchWrongValues2

 recover from several, monadic
   the first is catched                      ${ForCatchingEffMonadic.catchFirstWrongValue}
   all are catched                           ${ForCatchingEffMonadic.catchAllWrongValues}
   the last is catched                       ${ForCatchingEffMonadic.catchLastWrongValue}

 recover from several, applicative
   the first is catched                      ${ForCatchingEffApplicative.catchFirstWrongValue}
   all are catched                           ${ForCatchingEffApplicative.catchAllWrongValues}
   the last is catched                       ${ForCatchingEffApplicative.catchLastWrongValue}

 recover, the whole list is catched          $catchListOfWrongValues

 run is stack safe with Validate  $stacksafeRun

"""
  type S = Fx.fx1[ValidateString]
  type ValidateString[A] = Validate[String, A]

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

  def validateWarn = {
    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.warning[S, String]("warning1")
        a <- ValidateEffect.warning[S, String, Int](10, "warning2")
      } yield a

    validate.runIorNel.run ==== Ior.Both(NonEmptyList.of("warning1", "warning2"), 10)
  }

  def validateWarnAndErr = {
    val validate: Eff[S, Int] =
      for {
        a <- ValidateEffect.warning[S, String, Int](10, "warning")
        _ <- ValidateEffect.wrong[S, String]("error")
      } yield a

    validate.runIorNel.run ==== Ior.Left(NonEmptyList.of("warning", "error"))
  }

  def validateErrAndWarn = {
    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.wrong[S, String]("error")
        a <- ValidateEffect.warning[S, String, Int](10, "warning")
      } yield a

    validate.runIorNel.run ==== Ior.Left(NonEmptyList.of("error", "warning"))
  }
  def validateIorOk = {
    val validate: Eff[S, Int] =
      for {
        a <- ValidateEffect.correct[S, String, Int](1)
        b <- ValidateEffect.correct[S, String, Int](2)
      } yield a + b

    validate.runIorNel.run ==== Ior.Right(3)
  }

  def catchWrongValues1 = {
    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.wrong[S, String]("error!")
        a <- EffMonad[S].pure(3)
      } yield a

    validate.catchFirstWrong((s: String) => pure(4)).runNel.run ==== Right(4)
  }

  def catchWrongValues2 = {
    type E = String
    type Comput = Fx.fx2[Validate[E, *], Writer[E,*]]
    type Check[A] = Eff[Comput, A]

    val handle: E => Check[Unit] = { case e => tell[Comput, E](e).as(()) }

    val comp1: Check[Int] = for {
      _ <- wrong[Comput, E]("1").catchFirstWrong(handle)
      _ <- wrong[Comput, E]("2").catchFirstWrong(handle)
    } yield 0

    val comp2: Check[Int] = comp1

    comp2.runNel.runWriter.run ==== ((Right(0), List("1", "2")))
  }

  private def smashNelOfStrings[R](ss: NonEmptyList[String]): Eff[R, String] = pure(ss.mkString_("", ", ", ""))

  object ForCatchingEffMonadic {
    val intermediate: Eff[S, Unit] = for {
      _ <- ValidateEffect.wrong[S, String]("error1")
      _ <- ValidateEffect.wrong[S, String]("error1.5")
    } yield ()

    val v: Eff[S, String] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- intermediate
        a <- EffMonad[S].pure(3)
        _ <- ValidateEffect.wrong[S, String]("error2")
      } yield a.toString

    def catchFirstWrongValue = {
      v.catchFirstWrong((s: String) => pure(s)).runNel.run ==== Right("error1")
    }

    def catchAllWrongValues = {
      v.catchAllWrongs(smashNelOfStrings).runNel.run ==== Right("error1, error1.5, error2")
    }

    def catchLastWrongValue = {
      v.catchLastWrong((s: String) => pure(s)).runNel.run ==== Right("error2")
    }
  }

  object ForCatchingEffApplicative {
    val v1: Eff[S, Int] = ValidateEffect.validateValue(condition = true, 5, "no error")
    val v2: Eff[S, String] = for {
      x <- ValidateEffect.validateValue(condition = false, "str", "error1")
      _ <- ValidateEffect.wrong("error1.5")
    } yield x
    val v3: Eff[S, Int] = ValidateEffect.validateValue(condition = false, 6, "error2")

    final case class Prod(x: Int, s: String, y: Int)

    val prod: Eff[S, Prod] = (v1, v2, v3).mapN(Prod)
    val v: Eff[S, String] = prod.map(_.toString)

    def catchFirstWrongValue = {
      v.catchFirstWrong((s: String) => pure(s)).runNel.run ==== Right("error1")
    }

    def catchAllWrongValues = {
      v.catchAllWrongs(smashNelOfStrings).runNel.run ==== Right("error1, error1.5, error2")
    }

    def catchLastWrongValue = {
      v.catchLastWrong((s: String) => pure(s)).runNel.run ==== Right("error2")
    }
  }

  def catchListOfWrongValues = {
    type C = Fx.fx2[ValidateString, List]
    val validate: Eff[C, String] =
      for {
        v <- ListEffect.values[C, Int](1, 2)
        _ <- ValidateEffect.wrong[C, String]("error" + v.toString)
        a <- EffMonad[C].pure(3)
      } yield a.toString

    validate.runList.catchAllWrongs((ss: NonEmptyList[String]) => pure(ss.toList)).runNel.run ==== Right(List("error1", "error2"))
  }

  def stacksafeRun = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => ValidateEffect.wrong[S, String](i.toString))

    action.runNel.run must not(throwAn[Exception])
  }

}

