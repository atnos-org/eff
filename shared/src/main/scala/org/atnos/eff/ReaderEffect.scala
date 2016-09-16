package org.atnos.eff

import cats._
import data._
import Xor._
import Interpret._
import Eff._
import cats.~>

/**
 * Effect for computations depending on an environment.
 *
 * The inside datatype for this effect is cats.data.Reader
 */
trait ReaderEffect extends
  ReaderCreation with
  ReaderInterpretation

object ReaderEffect extends ReaderEffect

trait ReaderCreation {

  /** get the environment */
  def ask[R, T](implicit member: Reader[T, ?] |= R): Eff[R, T] =
    local[R, T, T](identity)

  /** modify the environment */
  def local[R, T, U](f: T => U)(implicit member: Reader[T, ?] |= R): Eff[R, U] =
    send[Reader[T, ?], R, U](Reader(f))

}

object ReaderCreation extends ReaderCreation

trait ReaderInterpretation {
  /** interpret the Reader effect by providing an environment when required */
  def runReader[R, U, A, B](env: A)(r: Eff[R, B])(implicit m: Member.Aux[Reader[A, ?], R, U]): Eff[U, B] = {
    val recurse = new Recurse[Reader[A, ?], U, B] {
      def apply[X](m: Reader[A, X]) = left(m.run(env))
      def applicative[X](ms: List[Reader[A, X]]): List[X] Xor Reader[A, List[X]] =
        Xor.Left(ms.map(_.run(env)))
    }

    interpret1[R, U, Reader[A, ?], B, B]((b: B) => b)(recurse)(r)
  }

  /**
   * Lift a computation over a "small" reader (for a subsystem) into
   * a computation over a "bigger" reader (for the full application)
   */
  def localReader[R, U, S, B, A](e: Eff[R, A], getter: B => S)
                             (implicit sr: Member.Aux[Reader[S, ?], R, U], br: (Reader[B, ?]) |= U): Eff[U, A] =
    translate(e) {
      new Translate[Reader[S, ?], U] {
        def apply[X](r: Reader[S, X]): Eff[U, X] =
          send[Reader[B, ?], U, X](Reader((b: B) => r.run(getter(b))))
      }
    }

  /**
   * Modify the read value
   */
  def modifyReader[R1, R2, U, S, T, A](e: Eff[R1, A])(f: T => S)(
    implicit readerS: Member.Aux[Reader[S, ?], R1, U],
             readerT: Member.Aux[Reader[T, ?], R2, U]): Eff[R2, A] =
    transform(e, new ~>[Reader[S, ?], Reader[T, ?]] {
      def apply[X](r: Reader[S, X]): Reader[T, X] =
        Reader((t: T) => r.run(f(t)))
    })
}

object ReaderInterpretation extends ReaderInterpretation

