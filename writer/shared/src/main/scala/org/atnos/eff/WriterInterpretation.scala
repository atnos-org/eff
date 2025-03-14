package org.atnos.eff

import cats.*
import cats.data.*
import cats.syntax.all.*
import org.atnos.eff.Eff.*
import org.atnos.eff.Interpret.*

trait WriterInterpretation {

  /**
   * run a writer effect and return the list of written values
   *
   * This uses a ListBuffer internally to append values
   */
  def runWriter[R, U, O, A, B](w: Eff[R, A])(implicit m: Member.Aux[Writer[O, *], R, U]): Eff[U, (A, List[O])] =
    runWriterFold(w)(ListFold[O])

  /**
   * More general fold of runWriter where we can use a fold to accumulate values in a mutable buffer
   */
  def runWriterFold[R, U, O, A, B](w: Eff[R, A])(fold: RightFold[O, B])(implicit m: Member.Aux[Writer[O, *], R, U]): Eff[U, (A, B)] = {
    val executed =
      Interpret.runInterpreter(w)(new Interpreter[Writer[O, *], U, A, (A, fold.S)] {
        def onPure(a: A): Eff[U, (A, fold.S)] =
          Eff.pure((a, fold.init))

        def onEffect[X](ox: Writer[O, X], continuation: Continuation[U, X, (A, fold.S)]): Eff[U, (A, fold.S)] = {
          val (o, x) = ox.run
          Eff.impure(x, continuation, { case (a, s) => (a, fold.fold(o, s)) })
        }

        def onLastEffect[X](x: Writer[O, X], continuation: Continuation[U, X, Unit]): Eff[U, Unit] =
          Eff.pure(())

        def onApplicativeEffect[X, T[_]: Traverse](xs: T[Writer[O, X]], continuation: Continuation[U, T[X], (A, fold.S)]): Eff[U, (A, fold.S)] = {
          val os = new collection.mutable.ListBuffer[O]
          val values = xs.map { w =>
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
  def runWriterUnsafe[R, U, O, A](w: Eff[R, A])(f: O => Unit)(implicit m: Member.Aux[Writer[O, *], R, U]): Eff[U, A] =
    interpretUnsafe(w)(new SideEffect[Writer[O, *]] {
      def apply[X](tx: Writer[O, X]): X = {
        val (o, x) = tx.run
        f(o)
        x
      }

      def applicative[X, Tr[_]: Traverse](ms: Tr[Writer[O, X]]): Tr[X] =
        ms.map(apply)
    })

  def runWriterEval[R, U, O, A](w: Eff[R, A])(f: O => Eval[Unit])(implicit m: Member.Aux[Writer[O, *], R, U], ev: Eval |= U): Eff[U, A] =
    runWriterFold(w)(EvalFold(f)).flatMap { case (a, e) => send[Eval, U, Unit](e).as(a) }

  def runWriterMonoid[R, U, O, A](w: Eff[R, A])(implicit m: Member.Aux[Writer[O, *], R, U], O: Monoid[O]): Eff[U, (A, O)] =
    runWriterFold(w)(MonoidFold[O])

  def runWriterIntoMonoid[R, U, O, M, A](w: Eff[R, A])(f: O => M)(implicit m: Member.Aux[Writer[O, *], R, U], M: Monoid[M]): Eff[U, (A, M)] =
    runWriterFold(w)(IntoMonoidFold[M, O](f))

  implicit def ListFold[A]: RightFold[A, List[A]] = new RightFold[A, List[A]] {
    type S = List[A]
    val init = List[A]()
    def fold(a: A, s: S) = a :: s
    def finalize(s: S) = s
  }

  def IntoMonoidFold[M: Monoid, A](f: A => M): RightFold[A, M] = new RightFold[A, M] {
    type S = M
    val init: M = Monoid[M].empty
    def fold(a: A, s: M): M = f(a) |+| s
    def finalize(s: M): M = s
  }

  def MonoidFold[A: Monoid]: RightFold[A, A] =
    IntoMonoidFold(identity)

  def EvalFold[A](f: A => Eval[Unit]): RightFold[A, Eval[Unit]] = new RightFold[A, Eval[Unit]] {
    type S = Eval[Unit]
    val init = Eval.Unit
    def fold(a: A, s: S) = Eval.defer { f(a) *> s }
    def finalize(s: S) = s
  }

}

object WriterInterpretation extends WriterInterpretation
