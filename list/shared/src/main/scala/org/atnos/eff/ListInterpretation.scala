package org.atnos.eff

import Eff._
import cats.syntax.all._
import Interpret._
import cats.Traverse

trait ListInterpretation {

  /** run an effect stack starting with a list effect */
  def runList[R, U, A](effect: Eff[R, A])(implicit m: Member.Aux[List, R, U]): Eff[U, List[A]] =
    runInterpreter(effect)(new Interpreter[List, U, A, List[A]] {
      def onPure(a: A): Eff[U, List[A]] =
        Eff.pure(List(a))

      def onEffect[X](xs: List[X], continuation: Continuation[U, X, List[A]]): Eff[U, List[A]] =
        xs.traverse(continuation).map(_.flatten)

      def onLastEffect[X](x: List[X], continuation: Continuation[U, X, Unit]): Eff[U, Unit] =
        Eff.pure(())

      def onApplicativeEffect[X, T[_]: Traverse](xs: T[List[X]], continuation: Continuation[U, T[X], List[A]]): Eff[U, List[A]] = {
        val sequenced: List[T[X]] = xs.sequence
        sequenced match {
          case Nil => continuation.runOnNone >> Eff.pure(Nil)
          case tx :: rest =>
            Eff.impure[U, T[X], List[A]](
              tx,
              Continuation.lift((tx1: T[X]) => continuation(tx1).flatMap(la => rest.map(continuation).sequence.map(ls => la ++ ls.flatten)))
            )
        }
      }
    })
}

object ListInterpretation extends ListInterpretation
