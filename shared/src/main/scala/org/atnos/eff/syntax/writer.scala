package org.atnos.eff.syntax

import cats.data.Writer
import org.atnos.eff.Tag._
import org.atnos.eff._

object writer extends writer

trait writer {

  implicit class WriterEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runWriter[O](implicit member: Member[Writer[O, ?], R]): Eff[member.Out, (A, List[O])] =
      WriterInterpretation.runWriter(e)(member.aux)

    def runWriterLog[O](implicit member: Member[Writer[O, ?], R]): Eff[member.Out, List[O]] =
      runWriter[O](member).map(_._2)

    def runWriterFold[O, B](fold: Fold[O, B])(implicit member: Member[Writer[O, ?], R]): Eff[member.Out, (A, B)] =
      WriterInterpretation.runWriterFold(e)(fold)(member.aux)

    def runWriterTagged[O, T](implicit member: Member[({type l[X] = Writer[O, X] @@ T})#l, R]): Eff[member.Out, (A, List[O])] =
      WriterInterpretation.runWriterTagged(e)(member.aux)
  }

}


