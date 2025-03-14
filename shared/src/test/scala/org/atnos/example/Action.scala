package org.atnos.example

import cats.Eval
import org.atnos.eff.*
import org.atnos.eff.Eff.run
import org.atnos.eff.ErrorEffect.*
import org.atnos.eff.EvalEffect.*
import org.atnos.example.ConsoleEffect.*
import org.atnos.example.WarningsEffect.*

/**
 * This is an example of a stack of effect with:
 *
 *  - 2 different writers: one for warnings, the other one for logging to the console
 *  - one "IO" effect
 *  - one Error effect
 */
object Action extends ActionCreation with ActionInterpretation

trait ActionTypes {
  type ActionStack = Fx.fx4[ErrorOrOk, Console, Warnings, Eval]

}

trait ActionCreation extends ActionTypes {

  /**
    * warn the user about something that is probably wrong on his side,
    * and then fail all other computations
    */
  def warnAndFail[R, A](message: String, failureMessage: String)(implicit m1: Warnings |= R, m2: ErrorOrOk |= R): Eff[R, A] =
    warn(message)(using m1) >>
      fail(failureMessage)
}
trait ActionInterpretation extends ActionImplicits {
  def runAction[A](action: Eff[ActionStack, A], printer: String => Unit = s => ()): (Error Either A, List[String]) =
    run(runEval(runWarnings(runConsoleToPrinter(printer)(runError(action)))))
}

object ActionCreation extends ActionCreation

trait ActionImplicits extends ActionTypes {

  implicit def ErrorOrOkMember: Member.Aux[ErrorOrOk, ActionStack, Fx3[Console, Warnings, Eval]] =
    Member.Member4L[ErrorOrOk, Console, Warnings, Eval]

  implicit def ConsoleMember
    : Member.Aux[Console, FxAppend[Fx1[ErrorOrOk], Fx3[Console, Warnings, Eval]], FxAppend[Fx1[ErrorOrOk], Fx2[Warnings, Eval]]] =
    Member.MemberAppendR[Console, Fx1[ErrorOrOk], Fx3[Console, Warnings, Eval], Fx2[Warnings, Eval]](using Member.Member3L[Console, Warnings, Eval])

  implicit def WarningsMember
    : Member.Aux[Warnings, FxAppend[Fx1[ErrorOrOk], Fx3[Console, Warnings, Eval]], FxAppend[Fx1[ErrorOrOk], Fx2[Console, Eval]]] =
    Member.MemberAppendR[Warnings, Fx1[ErrorOrOk], Fx3[Console, Warnings, Eval], Fx2[Console, Eval]](using Member.Member3M[Console, Warnings, Eval])

  implicit def EvalMember
    : Member.Aux[Eval, FxAppend[Fx1[ErrorOrOk], Fx3[Console, Warnings, Eval]], FxAppend[Fx1[ErrorOrOk], Fx2[Console, Warnings]]] =
    Member.MemberAppendR[Eval, Fx1[ErrorOrOk], Fx3[Console, Warnings, Eval], Fx2[Console, Warnings]](using Member.Member3R[Console, Warnings, Eval])
}

object ActionImplicits extends ActionImplicits
