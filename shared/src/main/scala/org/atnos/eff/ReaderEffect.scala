package org.atnos.eff

import cats._
import data._
import Xor._
import Interpret._
import Eff._

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
  def ask[R, T](implicit member: Reader[T, ?] <= R): Eff[R, T] =
    local[R, T, T](identity)

  /** get the environment */
  /** modify the environment */
  def local[R, T, U](f: T => U)(implicit member: Reader[T, ?] <= R): Eff[R, U] =
    send[Reader[T, ?], R, U](Reader(f))

}

object ReaderCreation extends ReaderCreation

trait ReaderInterpretation {
  /** interpret the Reader effect by providing an environment when required */
  def runReader[R <: Effects, U <: Effects, A, B](env: A)(r: Eff[R, B])(implicit m: Member.Aux[Reader[A, ?], R, U]): Eff[U, B] = {
    val recurse = new Recurse[Reader[A, ?], U, B] {
      def apply[X](m: Reader[A, X]) = left(m.run(env))
    }

    interpret1[R, U, Reader[A, ?], B, B]((b: B) => b)(recurse)(r)
  }

  /**
   * Lift a computation over a "small" reader (for a subsystem) into
   * a computation over a "bigger" reader (for the full application)
   */
  def localReader[SR, BR, U, S, B, A](r: Eff[SR, A], getter: B => S)
                                    (implicit sr: Member.Aux[Reader[S, ?], SR, U], br: Member.Aux[Reader[B, ?], BR, U]): Eff[BR, A] =
    transform[SR, BR, U, Reader[S, ?], Reader[B, ?], A](r, new ~>[Reader[S, ?], Reader[B, ?]] {
      def apply[X](r: Reader[S, X]): Reader[B, X] =
        Reader((b: B) => r.run(getter(b)))
    })
}

object ReaderInterpretation extends ReaderInterpretation

