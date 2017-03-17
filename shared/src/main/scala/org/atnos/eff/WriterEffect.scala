package org.atnos.eff

import scala.collection.mutable._
import cats._
import data._
import cats.implicits._
import Eff._
import Interpret._

/**
 * Effect for logging values alongside computations
 *
 * Compared to traditional Writer monad which accumulates values by default
 * this effect can be interpreted in different ways:
 *
 *  - log values to the console or to a file as soon as they are produced
 *  - accumulate values in a list
 *
 */
trait WriterEffect extends
  WriterCreation with
  WriterInterpretation

object WriterEffect extends WriterEffect

trait WriterCreation {

  /** write a given value */
  def tell[R, O](o: O)(implicit member: Writer[O, ?] |= R): Eff[R, Unit] =
    send[Writer[O, ?], R, Unit](Writer(o, ()))

}

object WriterCreation extends WriterCreation

trait WriterInterpretation {

  /**
   * run a writer effect and return the list of written values
   *
   * This uses a ListBuffer internally to append values
   */
  def runWriter[R, U, O, A, B](w: Eff[R, A])(implicit m: Member.Aux[Writer[O, ?], R, U]): Eff[U, (A, List[O])] =
    runWriterFold(w)(ListFold[O])

  /**
   * More general fold of runWriter where we can use a fold to accumulate values in a mutable buffer
   */
  def runWriterFold[R, U, O, A, B](w: Eff[R, A])(fold: RightFold[O, B])(implicit m: Member.Aux[Writer[O, ?], R, U]): Eff[U, (A, B)] = {
    val executed =
      Interpret.interpretGeneric(w)(new Interpreter[Writer[O, ?], U, A, (A, fold.S)] {
        def onPure(a: A): Eff[U, (A, fold.S)] =
          Eff.pure((a, fold.init))

        def onEffect[X](ox: Writer[O, X], continuation: X => Eff[U, (A, fold.S)]): Eff[U, (A, fold.S)] = {
          val (o, x) = ox.run
          Eff.impure(x, continuation, { case (a, s) => (a, fold.fold(o, s)) })
        }

        def onLastEffect[X](x: Writer[O, X], continuation: X => Eff[U, Unit]): Eff[U, Unit] =
          Eff.pure(())

        def onApplicativeEffect[X, T[_] : Traverse](xs: T[Writer[O, X]], continuation: T[X] => Eff[U, (A, fold.S)]): Eff[U, (A, fold.S)] = {
          val os = new collection.mutable.ListBuffer[O]
          val values = xs.map { w: Writer[O, X] =>
            val (o, x) = w.run
            os.append(o)
            x
          }

          Eff.impure(values, continuation, { case (a, s) => (a, os.toList.foldLeft(s) { (res, o) => fold.fold(o, res) }) })
        }

      })

    executed.map { case (a, s) => (a, fold.finalize(s)) }
  }

  /**
   * Run a side-effecting fold
   */
  def runWriterUnsafe[R, U, O, A](w: Eff[R, A])(f: O => Unit)(implicit m: Member.Aux[Writer[O, ?], R, U]): Eff[U, A] =
    runWriterFold(w)(UnsafeFold(f)).map(_._1)

  def runWriterEval[R, U, O, A](w: Eff[R, A])(f: O => Eval[Unit])(implicit m: Member.Aux[Writer[O, ?], R, U], ev: Eval |= U): Eff[U, A] =
    runWriterFold(w)(EvalFold(f)).flatMap { case (a, e) => send[Eval, U, Unit](e).as(a) }

  implicit def ListFold[A]: RightFold[A, List[A]] = new RightFold[A, List[A]] {
    type S = List[A]
    val init = List[A]()
    def fold(a: A, s: S) = a :: s
    def finalize(s: S) = s
  }

  def MonoidFold[A : Monoid]: RightFold[A, A] = new RightFold[A, A] {
    type S = A
    val init = Monoid[A].empty
    def fold(a: A, s: S) = a |+| s
    def finalize(s: S) = s
  }

  def UnsafeFold[A](f: A => Unit): RightFold[A, Unit] = new RightFold[A, Unit] {
    type S = Unit
    val init = ()
    def fold(a: A, s: S) = f(a)
    def finalize(s: S) = s
  }

  def EvalFold[A](f: A => Eval[Unit]): RightFold[A, Eval[Unit]] = new RightFold[A, Eval[Unit]] {
    type S = Eval[Unit]
    val init = Eval.now(())
    def fold(a: A, s: S) = s >> f(a)
    def finalize(s: S) = s
  }

}

/** support trait for folding values while possibly keeping some internal state */
trait RightFold[A, B] {
  type S
  val init: S
  def fold(a: A, s: S): S
  def finalize(s: S): B
}

object WriterInterpretation extends WriterInterpretation
