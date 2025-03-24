package org.atnos.eff.syntax

import cats.data.Kleisli
import cats.data.Reader
import org.atnos.eff.*

object reader extends reader

trait reader {

  given readerExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {

      def runReader[C](c: C)(using member: Member[Reader[C, *], R]): Eff[member.Out, A] =
        ReaderInterpretation.runReader(c)(e)(using member.aux)

      def runKleisli[U, S, F[_]](env: S)(using mx: Member.Aux[Kleisli[F, S, *], R, U], m: F |= U): Eff[U, A] =
        ReaderInterpretation.runKleisli[R, U, S, A, F](env)(e)(using mx, m)

      def translateReader[U, S, B](getter: B => S)(using m1: Member.Aux[Reader[S, *], R, U], m2: Reader[B, *] |= U): Eff[U, A] =
        ReaderInterpretation.translateReader[R, U, S, B, A](e, getter)(using m1, m2)

      def zoomReader[R2, U, S, T](f: T => S)(using Member.Aux[Reader[S, *], R, U], Member.Aux[Reader[T, *], R2, U]): Eff[R2, A] =
        ReaderInterpretation.zoomReader[R, R2, U, S, T, A](e)(f)

      def localReader[T](modify: T => T)(using r: Reader[T, *] /= R): Eff[R, A] =
        ReaderInterpretation.localReader(e)(modify)(using r)

    }
  }

}
