package org.specs2.control.eff

import scala.util.control.NonFatal
import cats.data._, Xor._
import Eff._
import Effects._
import Interpret._

/**
 * Effect for delayed computations
 *
 * uses cats.Eval as a supporting data structure
 *
 */
object EvalEffect {

   type Eval[A] = cats.Eval[A]

  def now[R, A](a: A)(implicit m: Member[Eval, R]): Eff[R, A] =
    pure(a)

  def delay[R, A](a: =>A)(implicit m: Member[Eval, R]): Eff[R, A] =
    send(cats.Eval.later(a))

  def runEval[R <: Effects, A](r: Eff[Eval[?] |: R, A]): Eff[R, A] = {
    val recurse = new Recurse[Eval, R, A] {
      def apply[X](m: Eval[X]) = Left(m.value)
    }

    interpret1((a: A) => a)(recurse)(r)
  }

  def attemptEval[R <: Effects, A](r: Eff[Eval[?] |: R, A]): Eff[R, Throwable Xor A] = {
    val recurse = new Recurse[Eval, R, Throwable Xor A] {
      def apply[X](m: Eval[X]) =
        try { Left(m.value) }
        catch { case NonFatal(t) => Right(Eff.pure(Left(t))) }
    }

    interpret1((a: A) => Right(a): Throwable Xor A)(recurse)(r)
  }

}

