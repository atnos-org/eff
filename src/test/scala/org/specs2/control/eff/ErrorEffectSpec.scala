package org.specs2.control.eff

import org.specs2.Specification
import Eff._
import Effects._
import ErrorEffect.{ok => OK, _}
import EvalEffect._
import WriterEffect._

import scala.collection.mutable.ListBuffer
import syntax.error._
import cats.syntax.all._
import cats.data._, Xor._

class ErrorEffectSpec extends Specification { def is = s2"""

 An action can be evaluated after another
   when the first action is ok   $andFinallyOk
   even if there is an exception $andFinallyKo

 An action can be evaluated, with another one
   if the first is successful, the second is not executed $orElse1
   if the first is not successful, the second is executed $orElse2

  Writer can be used with Error to get logs even if there is an exception $logException

"""

  type R = ErrorOrOk |: NoEffect

  def andFinallyOk = {

    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      OK[R, Unit](messages.append("first"))

    val all: Eff[R, Unit] =
      action.andFinally(OK(messages.append("final")))

    run(runError(all))

    messages.toList ==== List("first", "final")
  }

  def andFinallyKo = {

    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      OK[R, Unit] { throw new Exception("boom"); messages.append("first") }

    val all: Eff[R, Unit] =
      action.andFinally(OK(messages.append("final")))

    run(runError(all))

    messages.toList ==== List("final")
  }

  def orElse1 = {
    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      OK[R, Int] { messages.append("first"); 1 }

    val all: Eff[R, Int] =
      action.orElse(OK { messages.append("second"); 2 })

    (run(runError(all)) ==== Right(1)) and
    (messages.toList ==== List("first"))
  }

  def orElse2 = {
    val messages: ListBuffer[String] = new ListBuffer[String]

    val action =
      OK[R, Int] { throw new Exception("boom"); messages.append("first"); 1 }

    val all: Eff[R, Int] =
      action.orElse(OK { messages.append("second"); 2 })

    (run(runError(all)) ==== Right(2)) and
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
      run(runEval(runWriter(runError(action))))

    (result._2 ==== List("start")) and
    (result._1.toErrorSimpleMessage ==== Option("Error[java.lang.Exception] boom"))
  }

}

