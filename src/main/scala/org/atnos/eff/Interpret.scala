package org.atnos.eff

import cats.data._, Xor._
import Eff._

/**
 * Support methods to create an interpreter (or "effect handlers") for a given Eff[M |: R, A].
 * The aim being to "consume" just that effect and produce a value of type B with possibly other effects: Eff[R, B]
 *
 * Those methods guarantee a stack-safe behaviour when running on a large list of effects
 * (in a list.traverseU(f) for example).
 *
 * There are 3 different types of supported interpreters:
 *
 *  1. interpret + Recurse
 *
 *  This interpreter is used to handle effects which either return a value X from M[X] or stops with Eff[R, B]
 *  See an example of such an interpreter in Eval where we just evaluate a computation X for each Eval[X].
 *
 *  2. interpretState + StateRecurse
 *
 *  This interpreter is used to handle effects which either return a value X from M[X] or stops with Eff[R, B]
 *
 *  3. interpretLoop + Loop
 *
 *  The most generic kind of interpreter where we can even recurse in the case of Pure(a) (See ListEffect for such a use)
 */
trait Interpret {

  /**
   * Helper trait for computations
   * which might produce several M[X] in a stack of effects.
   *
   * Either we can produce an X to pass to a continuation or we're done
   */
  trait Recurse[M[_], R, A] {
    def apply[X](m: M[X]): X Xor Eff[R, A]
  }

  /**
   * interpret the effect M in the M |: R stack
   */
  def interpret[R <: Effects, U <: Effects, M[_], A, B](pure: A => Eff[U, B], recurse: Recurse[M, U, B])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] = {
    val loop = new Loop[M, R, A, Eff[U, B]] {
      type S = Unit
      val init = ()

      def onPure(a: A, s: Unit): (Eff[R, A], Unit) Xor Eff[U, B] =
        Right(pure(a))

      def onEffect[X](mx: M[X], continuation: Arrs[R, X, A], s: Unit): (Eff[R, A], Unit) Xor Eff[U, B] =
        recurse(mx).bimap(x => (continuation(x), ()), identity _)
    }
    interpretLoop[R, U, M, A, B, Unit](pure, loop)(effects)
  }

  /**
   * simpler version of interpret where the pure value is just mapped to another type
   */
  def interpret1[R <: Effects, U <: Effects, M[_], A, B](pure: A => B)(recurse: Recurse[M, U, B])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] =
    interpret[R, U, M, A, B]((a: A) => EffMonad[U].pure(pure(a)), recurse)(effects)

  /**
   * Helper trait for computations
   * which might produce several M[X] in a stack of effects and which need to keep some state around
   *
   * This is typically the case for Writer or State which need to keep some state S after each evaluation
   * Evaluating the effect M[X] might use the previous S value as shown in the `apply method`
   *
   * Finally when the Eff[M |: R, A] returns an A, this one can be combined with the last state value to produce a B
   *
   */
  trait StateRecurse[M[_], A, B] {
    type S
    val init: S
    def apply[X](x: M[X], s: S): (X, S)
    def finalize(a: A, s: S): B
  }

  /**
   * interpret the effect M in the M |: R stack, keeping track of some state
   */
  def interpretState[R <: Effects, U <: Effects, M[_], A, B](pure: A => Eff[U, B], recurse: StateRecurse[M, A, B])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] = {
    val loop = new Loop[M, R, A, Eff[U, B]] {
      type S = recurse.S
      val init: S = recurse.init

      def onPure(a: A, s: S): (Eff[R, A], S) Xor Eff[U, B] =
        Right(EffMonad[U].pure(recurse.finalize(a, s)))

      def onEffect[X](mx: M[X], continuation: Arrs[R, X, A], s: S): (Eff[R, A], S) Xor Eff[U, B] =
        Left { recurse(mx, s) match { case (a, b) => (continuation(a), b)} }
    }
    interpretLoop(pure, loop)(effects)
  }

  /**
   * simpler version of interpret1 where the pure value is just mapped to another type
   */
  def interpretState1[R <: Effects, U <: Effects, M[_], A, B](pure: A => B)(recurse: StateRecurse[M, A, B])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] =
    interpretState((a: A) => EffMonad[U].pure(pure(a)), recurse)(effects)

  /**
   * Generalisation of Recurse and StateRecurse
   */
  trait Loop[M[_], R <: Effects, A, B] {
    type S
    val init: S
    def onPure(a: A, s: S): (Eff[R, A], S) Xor B
    def onEffect[X](x: M[X], continuation: Arrs[R, X, A], s: S): (Eff[R, A], S) Xor B
  }

  /**
   * generalization of interpret and interpretState
   *
   * This method contains a loop which is stack-safe
   */
  def interpretLoop[R <: Effects, U <: Effects, M[_], A, B, S](pure: A => Eff[U, B], loop: Loop[M, R, A, Eff[U, B]])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] = {
    def go(eff: Eff[R, A], s: loop.S): Eff[U, B] = {
      eff match {
        case Pure(a) =>
          loop.onPure(a, s) match {
            case Left((a1, s1)) => go(a1, s1)
            case Right(b) => b
          }

        case Impure(union, continuation) =>
          m.project(union) match {
            case Right(v) =>
              loop.onEffect(v, continuation, s) match {
                case Left((x, s1)) => go(x, s1)
                case Right(b)      => b
              }

            case Left(u) =>
              Impure[U, union.X, B](u, Arrs.singleton(x => go(continuation(x), s)))
          }
      }
    }

    go(effects, loop.init)
  }

  def interpretLoop1[R <: Effects, U <: Effects, M[_], A, B, S](pure: A => B)(loop: Loop[M, R, A, Eff[U, B]])(effects: Eff[R, A])(implicit m: Member.Aux[M, R, U]): Eff[U, B] =
    interpretLoop[R, U, M, A, B, S]((a: A) => EffMonad[U].pure(pure(a)), loop)(effects)

}

object Interpret extends Interpret

