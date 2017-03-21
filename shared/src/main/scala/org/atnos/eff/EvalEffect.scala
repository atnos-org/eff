package org.atnos.eff

import scala.util.control.NonFatal
import cats._
import cats.implicits._
import Eff._
import Interpret._

/**
 * Effect for delayed computations
 *
 * uses cats.Eval as a supporting data structure
 */
trait EvalEffect extends
  EvalTypes with
  EvalCreation with
  EvalInterpretation

object EvalEffect extends EvalEffect

trait EvalTypes {
  type _Eval[R] = Eval <= R
  type _eval[R] = Eval |= R
}

object EvalTypes extends EvalTypes

trait EvalCreation extends EvalTypes {
  def now[R :_eval, A](a: A): Eff[R, A] =
    pure(a)

  def delay[R :_eval, A](a: => A): Eff[R, A] =
    send(cats.Eval.later(a))

  def defer[R :_eval, A](eff: =>Eval[Eff[R, A]]): Eff[R, A] = {
    send(cats.Eval.defer(eff)).flatten
  }
}

trait EvalInterpretation extends EvalTypes {

  def runEval[R, U, A](effect: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, A] =
    recurse(effect)(new Recurser[Eval, U, A, A] {
      def onPure(a: A): A =
        a

      def onEffect[X](e: Eval[X]): X Either Eff[U, A] =
        Left(e.value)

      def onApplicative[X, T[_]: Traverse](ms: T[Eval[X]]): T[X] Either Eval[T[X]] =
        Right(Eval.later(ms.map(_.value)))
    })

  def attemptEval[R, U, A](effect: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, Throwable Either A] =
   recurse(effect)(new Recurser[Eval, U, A, Throwable Either A] {
     def onPure(a: A): Throwable Either A =
       Right(a)

     def onEffect[X](e: Eval[X]): X Either Eff[U, Throwable Either A] =
       try { Left(e.value) }
       catch { case NonFatal(t) => Right(Eff.pure(Left(t))) }

     def onApplicative[X, T[_]: Traverse](ms: T[Eval[X]]): T[X] Either Eval[T[X]] =
       Right(ms.sequence)
    })
}

object EvalInterpretation extends EvalInterpretation

