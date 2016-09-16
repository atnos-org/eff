package org.atnos.eff

import scala.util.control.NonFatal
import cats.data._, Xor._
import cats.Eval
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
}

trait EvalInterpretation extends EvalTypes {
  def runEval[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, A] = {
    val recurse = new Recurse[Eval, U, A] {
      def apply[X](m: Eval[X]) = Left(m.value)
      def applicative[X](ms: List[Eval[X]]): List[X] Xor Eval[List[X]] = Xor.Left(ms.map(_.value))
    }

    interpret1((a: A) => a)(recurse)(r)
  }

  def attemptEval[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, Throwable Xor A] = {
    val recurse = new Recurse[Eval, U, Throwable Xor A] {
      def apply[X](m: Eval[X]) =
        try { Left(m.value) }
        catch { case NonFatal(t) => Right(Eff.pure(Left(t))) }

      def applicative[X](ms: List[Eval[X]]): List[X] Xor Eval[List[X]] =
        Xor.Right(Eval.later(ms.map(_.value)))
    }

    interpret1((a: A) => Right(a): Throwable Xor A)(recurse)(r)
  }
}

object EvalInterpretation extends EvalInterpretation

