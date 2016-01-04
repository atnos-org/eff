package org.specs2.example

import org.specs2.control.eff._
import Effects._, Eff._
import cats.syntax.all._
import cats.data._
import WarningsEffect._
import ConsoleEffect._
import EvalEffect._
import ErrorEffect._
import MemberNat._
import Member.{<=}

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
 *  Error |: Console |: Warnings |: Eval |: EffectsNil
 *
 *  will return warnings *and* failures: (String Xor A, Vector[String])
 *
 * Whereas
 *
 *  Console |: Warnings |: Error |: Eval |: EffectsNil
 *
 *  will return not warnings if there is a failure: String Xor (A, Vector[String])
 *
 * Also note that Eval is the last effect which means that nothing get evaluated until we run the last interpreter
 *
 */
object Action {

  type ActionStack = ErrorOrOk |: Console |: Warnings |: Eval |: NoEffect

  implicit def EvalMember: Member[Eval, ActionStack] =
    Member.MemberNatIsMember

  implicit def WarningsMember: Member[Warnings, ActionStack] =
    Member.MemberNatIsMember

  implicit def ConsoleMember: Member[Console, ActionStack] =
    Member.MemberNatIsMember

  implicit def ErrorMember: Member[ErrorOrOk, ActionStack] =
    Member.MemberNatIsMember

  /**
   * warn the user about something that is probably wrong on his side,
   * and then fail all other computations
   */
  def warnAndFail[R <: Effects, A](message: String, failureMessage: String)(implicit m1: Warnings <= R, m2: ErrorOrOk <= R): Eff[R, A] =
    warn(message)(m1) >>
    fail(failureMessage)

  def runAction[A](action: Eff[ActionStack, A], printer: String => Unit = s => ()): (Error Xor A, List[String]) =
    run(runEval(runWarnings(runConsoleToPrinter(printer)(runError(action)))))


}
