package org.specs2.control.eff

import Eff._
import Effects._
import cats._, data._, Xor._
import Interpret._
import Tag._

/**
 * Effect for computations depending on an environment.
 *
 * The inside datatype for this effect is scalaz.Reader
 *
 * Several Reader effects can be present in a given stack provided that they are tagged with scala.Tag.
 *
 * A tagged Reader effect can be run with runTaggedReader
 *
 */
object ReaderEffect {

  /** get the environment */
  def ask[R, T](implicit member: Member[Reader[T, ?], R]): Eff[R, T] =
    local[R, T, T](identity)

  /** get the environment */
  def askTagged[R, Tg, T](implicit member: Member[({type l[X] = Reader[T, X] @@ Tg})#l, R]): Eff[R, T] =
    localTagged[R, Tg, T, T](identity)

  /** modify the environment */
  def local[R, T, U](f: T => U)(implicit member: Member[Reader[T, ?], R]): Eff[R, U] =
    send[Reader[T, ?], R, U](Reader(f))

  /** modify the environment */
  def localTagged[R, Tg, T, U](f: T => U)(implicit member: Member[({type l[X] = Reader[T, X] @@ Tg})#l, R]): Eff[R, U] =
    send[({type l[X] = Reader[T, X] @@ Tg})#l, R, U](Tag(Reader(f)))

  /** interpret the Reader effect by providing an environment when required */
  def runReader[R <: Effects, A, B](env: A)(r: Eff[Reader[A, ?] |: R, B]): Eff[R, B] = {
    val recurse = new Recurse[Reader[A, ?], R, B] {
      def apply[X](m: Reader[A, X]) = left(m.run(env))
    }

    interpret1[R, Reader[A, ?], B, B]((b: B) => b)(recurse)(r)
  }

  /** interpret a tagged Reader effect by providing an environment when required */
  def runTaggedReader[R <: Effects, T, A, B](env: A)(r: Eff[({type l[X] = Reader[A, X] @@ T})#l |: R, B]): Eff[R, B] = {
    val recurse = new Recurse[({type l[X] = Reader[A, X] @@ T})#l, R, B] {
      def apply[X](m: Reader[A, X] @@ T) = Left(Tag.unwrap(m).run(env))
    }

    interpret1[R, ({type l[X] = Reader[A, X] @@ T})#l, B, B]((b: B) => b)(recurse)(r)
  }

}
