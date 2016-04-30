package org.atnos.eff.syntax

import cats.data.Writer
import org.atnos.eff.Tag._
import org.atnos.eff._

object writer extends writer

trait writer {

  implicit class WriterEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runWriter[O, U <: Effects](implicit member: Member.Aux[Writer[O,  ?], R, U]): Eff[U, (A, List[O])] =
      WriterInterpretation.runWriter(e)

    def runWriterFold[O, B, U <: Effects](fold: Fold[O, B])(implicit member: Member.Aux[Writer[O,  ?], R, U]): Eff[U, (A, B)] =
      WriterInterpretation.runWriterFold(e)(fold)

    def runWriterTagged[O, U <: Effects, T](implicit member: Member.Aux[({type l[X] = Writer[O, X] @@ T})#l, R, U]): Eff[U, (A, List[O])] =
      WriterInterpretation.runWriterTagged(e)
  }

}


