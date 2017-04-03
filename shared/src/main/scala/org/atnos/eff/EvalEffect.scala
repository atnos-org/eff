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
    interpret.runInterpreter(effect)(new Interpreter[Eval, U, A, A] {
      def onPure(a: A): Eff[U, A] =
        pure(a)

      def onEffect[X](x: Eval[X], continuation: Continuation[U, X, A]): Eff[U, A] =
        Eff.impure(x.value, continuation)

      def onLastEffect[X](x: Eval[X], continuation: Continuation[U, X, Unit]): Eff[U, Unit] =
        Eff.impure(x.value, continuation)

      def onApplicativeEffect[X, T[_] : Traverse](xs: T[Eval[X]], continuation: Continuation[U, T[X], A]): Eff[U, A] =
        Eff.impure(xs.map(_.value), continuation)
    })

  def attemptEval[R, U, A](effect: Eff[R, A])(implicit m: Member.Aux[Eval, R, U]): Eff[U, Throwable Either A] =
    interpret.runInterpreter(effect)(new Interpreter[Eval, U, A, Throwable Either A] {
      def onPure(a: A): Eff[U, Throwable Either A] =
        pure(Right(a))

      def onEffect[X](x: Eval[X], continuation: Continuation[U, X, Throwable Either A]): Eff[U, Throwable Either A] =
        try { Eff.impure(x.value, continuation) }
        catch { case NonFatal(t) => Eff.pure(Left(t)) }

      def onLastEffect[X](x: Eval[X], continuation: Continuation[U, X, Unit]): Eff[U, Unit] =
        Eff.impure(x.value, continuation)

      def onApplicativeEffect[X, T[_] : Traverse](xs: T[Eval[X]], continuation: Continuation[U, T[X], Throwable Either A]): Eff[U, Throwable Either A] =
        Eff.impure(xs.map(_.value), continuation)
    })

  /** the monad error instance for Eval is useful for using detach on Eff[Fx1[Eval], A] */
  implicit final val monadErrorEval: MonadError[Eval, Throwable] = new MonadError[Eval, Throwable] {
    private val m: Monad[Eval] = Eval.catsBimonadForEval
    
    def pure[A](x: A): Eval[A] =
      m.pure(x)

    def flatMap[A, B](fa: Eval[A])(f: (A) => Eval[B]) =
      m.flatMap(fa)(f)

    def tailRecM[A, B](a: A)(f: (A) => Eval[Either[A, B]]): Eval[B] =
      m.tailRecM(a)(f)

    def raiseError[A](e: Throwable): Eval[A] =
      Eval.later(throw e)

    def handleErrorWith[A](fa: Eval[A])(f: Throwable => Eval[A]): Eval[A] =
      Eval.later {
        try Eval.now(fa.value)
        catch { case t: Throwable => f(t)}
      }.flatten

  }

}

object EvalInterpretation extends EvalInterpretation

