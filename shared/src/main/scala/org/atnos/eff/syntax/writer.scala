package org.atnos.eff.syntax

import cats.Eval
import cats.data.Writer
import org.atnos.eff._, eff._

object writer extends writer

trait writer {

  implicit class WriterEffectOps[R, A](e: Eff[R, A]) {

    def runWriter[O](implicit member: Member[Writer[O, ?], R]): Eff[member.Out, (A, List[O])] =
      WriterInterpretation.runWriter(e)(member.aux)

    def runWriterLog[O](implicit member: Member[Writer[O, ?], R]): Eff[member.Out, List[O]] =
      runWriter[O](member).map(_._2)

    def runWriterFold[O, B](fold: Fold[O, B])(implicit member: Member[Writer[O, ?], R]): Eff[member.Out, (A, B)] =
      WriterInterpretation.runWriterFold(e)(fold)(member.aux)

    def runWriterUnsafe[O](f: O => Unit)(implicit member: Member[Writer[O, ?], R]): Eff[member.Out, A] =
      WriterInterpretation.runWriterUnsafe(e)(f)(member.aux)

    def runWriterEval[O, U](f: O => Eval[Unit])(implicit member: Member.Aux[Writer[O, ?], R, U], v: Eval |= U): Eff[U, A] =
      WriterInterpretation.runWriterEval(e)(f)(member, v)
  }

}


