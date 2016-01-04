package org.specs2.control.eff

import Eff._
import Effects._
import cats._, state._, std.all._
import cats.syntax.functor._
import cats.syntax.flatMap._
import Tag._
import Interpret._

/**
 * Effect for passing state along computations
 *
 * Several state effects can be used in the same stack if they are tagged
 *
 * Internally backed up by scalaz.State
 *
 */
object StateEffect {

  /** store a new state value */
  def put[R, S](s: S)(implicit member: Member[State[S, ?], R]): Eff[R, Unit] =
    send[State[S, ?], R, Unit](State.set(s))

  /** get the current state value */
  def get[R, S](implicit member: Member[State[S, ?], R]): Eff[R, S] =
    send[State[S, ?], R, S](State.get)

  /** get the current state value and map it with a function f */
  def gets[R, S, T](f: S => T)(implicit member: Member[State[S, ?], R]): Eff[R, T] =
    send[State[S, ?], R, T](State.inspect(f))

  /** modify the current state value */
  def modify[R, S](f: S => S)(implicit member: Member[State[S, ?], R]): Eff[R, Unit] =
    get >>= ((s: S) => put(f(s)))

  /** run a state effect, with a Monoidal state */
  def evalZero[R <: Effects, S: Monoid, A](w: Eff[State[S, ?] |: R, A]): Eff[R, A] =
    eval(Monoid[S].empty)(w)

  /** run a state effect, with an initial value, return only the value */
  def eval[R <: Effects, S, A](initial: S)(w: Eff[State[S, ?] |: R, A]): Eff[R, A] =
    runState(initial)(w).map(_._1)

  /** run a state effect, with a monoidal state, return only the state */
  def execZero[R <: Effects, S : Monoid, A](w: Eff[State[S, ?] |: R, A]): Eff[R, S] =
    exec(Monoid[S].empty)(w)

  /** run a state effect, with an initial value, return only the state */
  def exec[R <: Effects, S, A](initial: S)(w: Eff[State[S, ?] |: R, A]): Eff[R, S] =
    runState(initial)(w).map(_._2)

  /** run a state effect, with an initial value */
  def runStateZero[R <: Effects, S : Monoid, A](w: Eff[State[S, ?] |: R, A]): Eff[R, (A, S)] =
    runState(Monoid[S].empty)(w)

  /** run a state effect, with an initial value */
  def runState[R <: Effects, S1, A](initial: S1)(w: Eff[State[S1, ?] |: R, A]): Eff[R, (A, S1)] = {
    val recurse: StateRecurse[State[S1, ?], A, (A, S1)] = new StateRecurse[State[S1, ?], A, (A, S1)] {
      type S = S1
      val init = initial

      def apply[X](x: State[S, X], s: S) =
        x.run(s).run.swap

      def finalize(a: A, s: S) = (a, s)

    }

    interpretState1[R, State[S1, ?], A, (A, S1)]((a: A) => (a, initial))(recurse)(w)
  }

  /** run a tagged state effect, with an initial value */
  def runTaggedState[R <: Effects, T, S1, A](initial: S1)(w: Eff[({type l[X] = State[S1, X] @@ T})#l |: R, A]): Eff[R, (A, S1)] = {
    type SS[X] = State[S1, X] @@ T

    val recurse: StateRecurse[SS, A, (A, S1)] = new StateRecurse[SS, A, (A, S1)] {
      type S = S1
      val init = initial

      def apply[X](x: State[S, X] @@ T, s: S) =
        Tag.unwrap(x).run(s).run.swap

      def finalize(a: A, s: S) = (a, s)

    }

    interpretState1[R, SS, A, (A, S1)]((a: A) => (a, initial))(recurse)(w)
  }
}
