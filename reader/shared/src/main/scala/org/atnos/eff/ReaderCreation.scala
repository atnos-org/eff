package org.atnos.eff

import cats._
import data._
import Eff._

trait ReaderCreation {

  /** get the environment */
  def ask[R, T](implicit member: Reader[T, *] |= R): Eff[R, T] =
    local[R, T, T](identity)

  /** modify the environment */
  def local[R, T, U](f: T => U)(implicit member: Reader[T, *] |= R): Eff[R, U] =
    send[Reader[T, *], R, U](Reader(f))

  /** modify the environment using a Kleisli[F, T, *] */
  def localKleisli[R, T, U, F[_]](f: T => F[U])(implicit member: Kleisli[F, T, *] |= R): Eff[R, U] =
    send[Kleisli[F, T, *], R, U](Kleisli(f))

}

object ReaderCreation extends ReaderCreation
