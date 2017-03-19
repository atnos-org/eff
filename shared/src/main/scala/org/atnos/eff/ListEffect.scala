package org.atnos.eff

import Eff._

import scala.collection.mutable.ListBuffer
import cats.implicits._
import Interpret._
import cats.Traverse

/**
 * Effect for computations possibly returning several values
 */
trait ListEffect extends
  ListCreation with
  ListInterpretation

object ListEffect extends ListEffect

trait ListCreation {

  type _List[R] = List <= R
  type _list[R] = List |= R

  /** create a list effect with no values */
  def empty[R :_list, A]: Eff[R, A] =
    fromList(List())

  /** create a list effect from a single value */
  def singleton[R :_list, A](a: A): Eff[R, A] =
    fromList(List(a))

  /** create a list effect from a list of values */
  def values[R :_list, A](as: A*): Eff[R, A] =
    fromList(as.toList)

  /** create a list effect from a list of values */
  def fromList[R :_list, A](as: List[A]): Eff[R, A] =
    send[List, R, A](as)
}

object ListCreation extends ListCreation

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

      def onApplicativeEffect[X, T[_] : Traverse](xs: T[List[X]], continuation: Continuation[U, T[X], List[A]]): Eff[U, List[A]] = {
        val sequenced: List[T[X]] = xs.sequence
        sequenced match {
          case Nil => continuation.runOnNone >> Eff.pure(Nil)
          case tx :: rest  => Eff.impure[U, T[X], List[A]](tx, Continuation.lift((tx1: T[X]) =>
            continuation(tx1).flatMap(la => rest.map(continuation).sequence.map(ls => la ++ ls.flatten))))
        }
      }
    })
}

object ListInterpretation extends ListInterpretation
