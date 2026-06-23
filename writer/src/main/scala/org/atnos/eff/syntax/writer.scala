package org.atnos.eff.syntax

import cats.Eval
import cats.Monoid
import cats.data.Writer
import org.atnos.eff.*

object writer extends writer

trait writer {

  given writerExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {

      def runWriter[O](using member: Member[Writer[O, *], R]): Eff[member.Out, (A, List[O])] =
        WriterInterpretation.runWriter(e)(using member.aux)

      def runWriterU[O, U](using member: Member.Aux[Writer[O, *], R, U]): Eff[U, (A, List[O])] =
        WriterInterpretation.runWriter(e)(using member)

      def runWriterNoLog[O](using member: Member[Writer[O, *], R]): Eff[member.Out, A] =
        runWriterUnsafe[O](_ => ())

      def runWriterNoLogU[O, U](using Member.Aux[Writer[O, *], R, U]): Eff[U, A] =
        runWriterUnsafe[O](_ => ())

      def discardWriter[O, U](using Member.Aux[Writer[O, *], R, U]): Eff[U, A] =
        runWriterNoLogU[O, U]

      def runWriterLog[O](using member: Member[Writer[O, *], R]): Eff[member.Out, List[O]] =
        runWriter[O](using member).map(_._2)

      def runWriterFold[O, B](fold: RightFold[O, B])(using member: Member[Writer[O, *], R]): Eff[member.Out, (A, B)] =
        WriterInterpretation.runWriterFold(e)(fold)(using member.aux)

      def runWriterMonoid[B](using member: Member[Writer[B, *], R], B: Monoid[B]): Eff[member.Out, (A, B)] =
        WriterInterpretation.runWriterMonoid(e)(using member.aux, B)

      def runWriterIntoMonoid[O, M](f: O => M)(using member: Member[Writer[O, *], R], M: Monoid[M]): Eff[member.Out, (A, M)] =
        WriterInterpretation.runWriterIntoMonoid(e)(f)(using member.aux, M)

      def runWriterUnsafe[O](f: O => Unit)(using member: Member[Writer[O, *], R]): Eff[member.Out, A] =
        WriterInterpretation.runWriterUnsafe(e)(f)(using member.aux)

      def runWriterEval[O, U](f: O => Eval[Unit])(using member: Member.Aux[Writer[O, *], R, U], v: Eval |= U): Eff[U, A] =
        WriterInterpretation.runWriterEval(e)(f)(using member, v)
    }
  }

}
