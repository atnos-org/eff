package org.atnos.eff

import scala.util.control.NonFatal
import cats.data._, Xor._
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
  type Eval[A] = cats.Eval[A]
}

object EvalTypes extends EvalTypes

trait EvalCreation extends EvalTypes {
  def now[R, A](a: A)(implicit m: Member[Eval, R]): Eff[R, A] =
    pure(a)

  def delay[R, A](a: => A)(implicit m: Member[Eval, R]): Eff[R, A] =
    send(cats.Eval.later(a))
}

trait EvalInterpretation extends EvalTypes {
  def runEval[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, A] = {
    val recurse = new Recurse[Eval, U, A] {
      def apply[X](m: Eval[X]) = Left(m.value)
    }

    interpret1((a: A) => a)(recurse)(r)
  }

  def attemptEval[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, Throwable Xor A] = {
    val recurse = new Recurse[Eval, U, Throwable Xor A] {
      def apply[X](m: Eval[X]) =
        try { Left(m.value) }
        catch { case NonFatal(t) => Right(Eff.pure(Left(t))) }
    }

    interpret1((a: A) => Right(a): Throwable Xor A)(recurse)(r)
  }
}

object EvalInterpretation extends EvalInterpretation

