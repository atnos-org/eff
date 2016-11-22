package org.atnos.eff

import scala.util.control.NonFatal
import cats._
import cats.data._
import cats.implicits._
import Either._
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
  def runEval[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, A] = {
    val recurse = new Recurse[Eval, U, A] {
      def apply[X](m: Eval[X]): X Either Eff[U, A] = Either.left(m.value)
      def applicative[X, T[_]: Traverse](ms: T[Eval[X]]): T[X] Either Eval[T[X]] = Right(Eval.later(ms.map(_.value)))
    }

    interpret1((a: A) => a)(recurse)(r)
  }

  def attemptEval[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, Throwable Either A] = {
    val recurse = new Recurse[Eval, U, Throwable Either A] {
      def apply[X](m: Eval[X]) =
        try { Left(m.value) }
        catch { case NonFatal(t) => Right(Eff.pure(Left(t))) }

      def applicative[X, T[_]: Traverse](ms: T[Eval[X]]): T[X] Either Eval[T[X]] =
        Right(Eval.later(ms.map(_.value)))
    }

    interpret1((a: A) => Right(a): Throwable Either A)(recurse)(r)
  }
}

object EvalInterpretation extends EvalInterpretation

