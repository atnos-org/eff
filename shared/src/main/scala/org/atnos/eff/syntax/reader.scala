package org.atnos.eff.syntax

import cats.data.Reader
import org.atnos.eff._

object reader extends reader

trait reader {

  implicit class ReaderEffectOps[R, A](e: Eff[R, A]) {

    def runReader[C](c: C)(implicit member: Member[Reader[C, ?], R]): Eff[member.Out, A] =
      ReaderInterpretation.runReader(c)(e)(member.aux)

    def translateReader[U, S, B](getter: B => S)(implicit m1: Member.Aux[Reader[S, ?], R, U],
                                                          m2: (Reader[B, ?]) |= U): Eff[U, A] =
      ReaderInterpretation.translateReader[R, U, S, B, A](e, getter)(m1, m2)

    def zoomReader[R2, U, S, T](f: T => S)(implicit readerS: Member.Aux[Reader[S, ?], R, U],
                                                    readerT: Member.Aux[Reader[T, ?], R2, U]): Eff[R2, A] =
      ReaderInterpretation.zoomReader[R, R2, U, S, T, A](e)(f)

    def localReader[T](modify: T => T)(implicit r: Reader[T, ?] /= R): Eff[R, A] =
      ReaderInterpretation.localReader(e)(modify)(r)

  }

}

