package org.atnos.eff

import org.specs2.Specification
import ErrorEffect.{ok => OK, ErrorOrOk}

import scala.collection.mutable.ListBuffer
import cats.data._, Xor._
import cats.Eval
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class ErrorEffectSpec extends Specification { def is = s2"""

 An action can be evaluated after another
   when the first action is ok   $andFinallyOk
   even if there is an exception $andFinallyKo

 An action can be evaluated, with another one
   if the first is successful, the second is not executed $orElse1
   if the first is not successful, the second is executed $orElse2

  Writer can be used with Error to get logs even if there is an exception $logException

 A thrown exception can be ignored $ignored

 An action with a given failure type can be translated to an action with another failure type $local
"""

  type R = ErrorOrOk |: NoEffect

  def andFinallyOk = {

    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      OK[R, Unit](messages.append("first"))

    val all: Eff[R, Unit] =
      action.andFinally(OK(messages.append("final")))

    all.runError.run

    messages.toList ==== List("first", "final")
  }

  def andFinallyKo = {

    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      OK[R, Unit] { throw new Exception("boom"); messages.append("first") }

    val all: Eff[R, Unit] =
      action.andFinally(OK(messages.append("final")))

    all.runError.run

    messages.toList ==== List("final")
  }

  def orElse1 = {
    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      OK[R, Int] { messages.append("first"); 1 }

    val all: Eff[R, Int] =
      action.orElse(OK { messages.append("second"); 2 })

    (all.runError.run ==== Right(1)) and
    (messages.toList ==== List("first"))
  }

  def orElse2 = {
    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      OK[R, Int] { throw new Exception("boom"); messages.append("first"); 1 }

    val all: Eff[R, Int] =
      action.orElse(OK { messages.append("second"); 2 })

    (all.runError.run ==== Right(2)) and
    (messages.toList ==== List("second"))
  }

  def logException = {

    type WriterString[A] = Writer[String, A]

    type E = ErrorOrOk |: WriterString |: Eval |: NoEffect

    val action: Eff[E, Int] = for {
      _ <- tell[E, String]("start")
      a <- OK[E, Int] { throw new Exception("boom"); 1 }
      _ <- tell[E, String]("end")
    } yield a

    val result =
      action.runError.runWriter.runEval.run

    (result._2 ==== List("start")) and
    (result._1.toErrorSimpleMessage ==== Option("Error[java.lang.Exception] boom"))
  }

  def ignored = {
    val action =
      OK[R, Int] { throw new IllegalArgumentException("boom"); 1 }

    val action2: Eff[R, Unit] =
      action.ignore[IllegalArgumentException]

    action2.runError.run ==== Right(())

  }

  def local = {
    case class Error1(m: String)
    object ErrorEffect1 extends ErrorEffect[Error1]

    case class Error2(e1: Error1)
    object ErrorEffect2 extends ErrorEffect[Error2]

    type R1 = ErrorEffect1.ErrorOrOk |: NoEffect
    implicit val m1: Member.Aux[ErrorEffect1.ErrorOrOk, R1, NoEffect] = Member.first

    type R2 = ErrorEffect2.ErrorOrOk |: NoEffect
    implicit val m2: Member.Aux[ErrorEffect2.ErrorOrOk, R2, NoEffect] = Member.first

    val action1: Eff[R1, Unit] =
      ErrorEffect1.fail(Error1("boom"))

    val action2: Eff[R2, Unit] =
      ErrorEffect.localError(action1, Error2)

    ErrorEffect2.runError(action2).run ==== Xor.left(Xor.right(Error2(Error1("boom"))))
  }

}

