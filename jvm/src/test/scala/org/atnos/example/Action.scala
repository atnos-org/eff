package org.atnos.example

import org.atnos.eff._
import Effects._, Eff._
import cats.syntax.all._
import cats.data._
import cats.Eval
import WarningsEffect._
import ConsoleEffect._
import EvalEffect._
import ErrorEffect._
import Member.<=

/**
 * This is an example of a stack of effect with:
 *
 *  - 2 different writers: one for warnings, the other one for logging to the console
 *  - one "IO" effect
 *  - one Error effect
 *
 * The order of the effects in the stack definition is important.
 *
 * For example
 *
 *  Error |: Console |: Warnings |: Eval |: NoEffect
 *
 *  will return warnings *and* failures: (String Xor A, Vector[String])
 *
 * Whereas
 *
 *  Console |: Warnings |: Error |: Eval |: NoEffect
 *
 *  will return not warnings if there is a failure: String Xor (A, Vector[String])
 *
 * Also note that Eval is the last effect which means that nothing get evaluated until we run the last interpreter
 *
 */
object Action extends ActionCreation with ActionInterpretation

trait ActionTypes {
  type ActionStack_ = ErrorOrOk |: Console |: Warnings |: Eval |: NoEffect
  val as = Fx[ActionStack_]
  type ActionStack = as.Fx
}

trait ActionCreation extends ActionTypes {
  /**
    * warn the user about something that is probably wrong on his side,
    * and then fail all other computations
    */
  def warnAndFail[R, A](message: String, failureMessage: String)(implicit m1: Warnings |= R, m2: ErrorOrOk |= R): Eff[R, A] =
    warn(message)(m1) >>
      fail(failureMessage)
}
trait ActionInterpretation extends ActionImplicits {
  def runAction[A](action: Eff[ActionStack, A], printer: String => Unit = s => ()): (Error Xor A, List[String]) =
    run(runEval(runWarnings(runConsoleToPrinter(printer)(runError(action)))))
}

object ActionCreation extends ActionCreation

trait ActionImplicits extends ActionTypes {

  implicit def ErrorOrOkMember: Member.Aux[ErrorOrOk, ActionStack, Fx3[Console, Warnings, Eval]] =
    Member.Member4L[ErrorOrOk, Console, Warnings, Eval]

  implicit def ConsoleMember =
    Member.MemberAppendR[Console, Fx1[ErrorOrOk], Fx3[Console, Warnings, Eval], Fx2[Warnings, Eval]](Member.Member3L[Console, Warnings, Eval])

  implicit def WarningsMember =
    Member.MemberAppendR[Warnings, Fx1[ErrorOrOk], Fx3[Console, Warnings, Eval], Fx2[Console, Eval]](Member.Member3M[Console, Warnings, Eval])

  implicit def EvalMember =
    Member.MemberAppendR[Eval, Fx1[ErrorOrOk], Fx3[Console, Warnings, Eval], Fx2[Console, Warnings]](Member.Member3R[Console, Warnings, Eval])
}

object ActionImplicits extends ActionImplicits
