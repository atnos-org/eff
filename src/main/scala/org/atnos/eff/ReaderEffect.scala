package org.atnos.eff

import cats._
import data._
import Xor._
import Interpret._
import Tag._
import Eff._
import Effects.|:

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
trait ReaderEffect extends
  ReaderCreation with
  ReaderInterpretation with
  ReaderImplicits

object ReaderEffect extends ReaderEffect

trait ReaderCreation {
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

  /** interpret a tagged Reader effect by providing an environment when required */
  def runReaderTagged[R <: Effects, U <: Effects, T, A, B](env: A)(r: Eff[R, B])(implicit
                                                                                 m: Member.Aux[({type l[X] = Reader[A, X] @@ T})#l, R, U]): Eff[U, B] = {

    val recurse = new Recurse[({type l[X] = Reader[A, X] @@ T})#l, U, B] {
      def apply[X](m: Reader[A, X] @@ T) = Left(Tag.unwrap(m).run(env))
    }

    interpret1[R, U, ({type l[X] = Reader[A, X] @@ T})#l, B, B]((b: B) => b)(recurse)(r)
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

trait ReaderImplicits extends ReaderImplicits1 {
  implicit def TaggedReaderMemberZero[Tg, A]: Member.Aux[({type l[X] = Reader[A, X] @@ Tg})#l, ({type l[X] = Reader[A, X] @@ Tg})#l |: NoEffect, NoEffect] = {
    type T[X] = Reader[A, X] @@ Tg
    Member.zero[T]
  }

  implicit def TaggedReaderMemberFirst[R <: Effects, Tg, A]: Member.Aux[({type l[X] = Reader[A, X] @@ Tg})#l, ({type l[X] = Reader[A, X] @@ Tg})#l |: R, R] = {
    type T[X] = Reader[A, X] @@ Tg
    Member.first[T, R]
  }
}

trait ReaderImplicits1 {
  implicit def TaggedReaderMemberSuccessor[O[_], R <: Effects, U <: Effects, Tg, A](implicit m: Member.Aux[({type l[X] = Reader[A, X] @@ Tg})#l, R, U]): Member.Aux[({type l[X] = Reader[A, X] @@ Tg})#l, O |: R, O |: U] = {
    type T[X] = Reader[A, X] @@ Tg
    Member.successor[T, O, R, U]
  }
}

object ReaderImplicits extends ReaderImplicits
