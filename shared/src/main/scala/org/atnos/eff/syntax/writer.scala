package org.atnos.eff.syntax

import cats.{Eval, Monoid}
import cats.data.Writer
import org.atnos.eff._

object writer extends writer

trait writer {

  implicit class WriterEffectOps[R, A](e: Eff[R, A]) {

    def runWriter[O](implicit member: Member[Writer[O, *], R]): Eff[member.Out, (A, List[O])] =
      WriterInterpretation.runWriter(e)(member.aux)

    def runWriterU[O, U](implicit member: Member.Aux[Writer[O, *], R, U]): Eff[U, (A, List[O])] =
      WriterInterpretation.runWriter(e)(member)

    def runWriterNoLog[O](implicit member: Member[Writer[O, *], R]): Eff[member.Out, A] =
      runWriterUnsafe[O](_ => ())

    def runWriterNoLogU[O, U](implicit member: Member.Aux[Writer[O, *], R, U]): Eff[U, A] =
      runWriterUnsafe[O](_ => ())

    def discardWriter[O, U](implicit member: Member.Aux[Writer[O, *], R, U]): Eff[U, A] =
      runWriterNoLogU[O, U]

    def runWriterLog[O](implicit member: Member[Writer[O, *], R]): Eff[member.Out, List[O]] =
      runWriter[O](member).map(_._2)

    def runWriterFold[O, B](fold: RightFold[O, B])(implicit member: Member[Writer[O, *], R]): Eff[member.Out, (A, B)] =
      WriterInterpretation.runWriterFold(e)(fold)(member.aux)

    def runWriterMonoid[B](implicit member: Member[Writer[B, *], R], B: Monoid[B]): Eff[member.Out, (A, B)] =
      WriterInterpretation.runWriterMonoid(e)(member.aux, B)

    def runWriterIntoMonoid[O, M](f: O => M)(implicit member: Member[Writer[O, *], R], M: Monoid[M]): Eff[member.Out, (A, M)] =
      WriterInterpretation.runWriterIntoMonoid(e)(f)(member.aux, M)

    def runWriterUnsafe[O](f: O => Unit)(implicit member: Member[Writer[O, *], R]): Eff[member.Out, A] =
      WriterInterpretation.runWriterUnsafe(e)(f)(member.aux)

    def runWriterEval[O, U](f: O => Eval[Unit])(implicit member: Member.Aux[Writer[O, *], R, U], v: Eval |= U): Eff[U, A] =
      WriterInterpretation.runWriterEval(e)(f)(member, v)
  }

}


