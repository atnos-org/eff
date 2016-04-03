package org.atnos.eff

import cats.syntax.functor._
import cats.syntax.flatMap._
import Tag._
import Interpret._
import Eff._
import cats._, data._
import Effects.|:

/**
 * Effect for passing state along computations
 *
 * Several state effects can be used in the same stack if they are tagged
 *
 * Internally backed up by cats.data.State
 *
 */
trait StateEffect extends
  StateCreation with
  StateInterpretation with
  StateImplicits

object StateEffect extends StateEffect

trait StateCreation {

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

  /** store a new state value */
  def putTagged[R, T, S](s: S)(implicit member: Member[({type l[X] = State[S, X] @@ T})#l, R]): Eff[R, Unit] =
    send[({type l[X] = State[S, X] @@ T})#l, R, Unit](Tag(State.set(s)))

  /** get the current state value */
  def getTagged[R, T, S](implicit member: Member[({type l[X] = State[S, X] @@ T})#l, R]): Eff[R, S] =
    send[({type l[X] = State[S, X] @@ T})#l, R, S](Tag(State.get))

  /** get the current state value and map it with a function f */
  def getsTagged[R, U, S, T](f: S => T)(implicit member: Member[({type l[X] = State[S, X] @@ U})#l, R]): Eff[R, T] =
    send[({type l[X] = State[S, X] @@ U})#l, R, T](Tag(State.inspect(f)))

  /** modify the current state value */
  def modifyTagged[R, U, T, S](f: S => S)(implicit member: Member[({type l[X] = State[S, X] @@ U})#l, R]): Eff[R, Unit] =
    getTagged >>= ((s: S) => putTagged(f(s)))

}

object StateCreation extends StateCreation

trait StateInterpretation {
  /** run a state effect, with a Monoidal state */
  def evalZero[R <: Effects, U <: Effects, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[State[S, ?], R, U]): Eff[U, A] =
    eval(Monoid[S].empty)(w)

  /** run a state effect, with an initial value, return only the value */
  def eval[R <: Effects, U <: Effects, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[State[S, ?], R, U]): Eff[U, A] =
    runState(initial)(w).map(_._1)

  /** run a state effect, with a monoidal state, return only the state */
  def execZero[R <: Effects, U <: Effects, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[State[S, ?], R, U]): Eff[U, S] =
    exec(Monoid[S].empty)(w)

  /** run a state effect, with an initial value, return only the state */
  def exec[R <: Effects, U <: Effects, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[State[S, ?], R, U]): Eff[U, S] =
    runState(initial)(w).map(_._2)

  /** run a state effect, with an initial value */
  def runStateZero[R <: Effects, U <: Effects, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[State[S, ?], R, U]): Eff[U, (A, S)] =
    runState(Monoid[S].empty)(w)

  /** run a state effect, with an initial value */
  def runState[R <: Effects, U <: Effects, S1, A](initial: S1)(w: Eff[R, A])(implicit m: Member.Aux[State[S1, ?], R, U]): Eff[U, (A, S1)] = {
    val recurse: StateRecurse[State[S1, ?], A, (A, S1)] = new StateRecurse[State[S1, ?], A, (A, S1)] {
      type S = S1
      val init = initial

      def apply[X](x: State[S, X], s: S) =
        x.run(s).value.swap

      def finalize(a: A, s: S) = (a, s)

    }

    interpretState1[R, U, State[S1, ?], A, (A, S1)]((a: A) => (a, initial))(recurse)(w)
  }


  /** run a state effect, with a Monoidal state */
  def evalTaggedZero[R <: Effects, U <: Effects, T, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, A] =
    evalTagged(Monoid[S].empty)(w)

  /** run a state effect, with an initial value, return only the value */
  def evalTagged[R <: Effects, U <: Effects, T, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, A] =
    runTaggedState(initial)(w).map(_._1)

  /** run a state effect, with a monoidal state, return only the state */
  def execTaggedZero[R <: Effects, U <: Effects, T, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, S] =
    execTagged(Monoid[S].empty)(w)

  /** run a state effect, with an initial value, return only the state */
  def execTagged[R <: Effects, U <: Effects, T, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, S] =
    runTaggedState(initial)(w).map(_._2)

  /** run a state effect, with an initial value */
  def runTaggedStateZero[R <: Effects, U <: Effects, T, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S, X] @@ T})#l, R, U]): Eff[U, (A, S)] =
    runTaggedState(Monoid[S].empty)(w)

  /** run a tagged state effect, with an initial value */
  def runTaggedState[R <: Effects, U <: Effects, T, S1, A](initial: S1)(w: Eff[R, A])(implicit m: Member.Aux[({type l[X] = State[S1, X] @@ T})#l, R, U]): Eff[U, (A, S1)] = {
    type SS[X] = State[S1, X] @@ T

    val recurse: StateRecurse[SS, A, (A, S1)] = new StateRecurse[SS, A, (A, S1)] {
      type S = S1
      val init = initial

      def apply[X](x: State[S, X] @@ T, s: S) =
        Tag.unwrap(x).run(s).value.swap

      def finalize(a: A, s: S) = (a, s)

    }

    interpretState1[R, U, SS, A, (A, S1)]((a: A) => (a, initial))(recurse)(w)
  }
}

object StateInterpretation extends StateInterpretation

trait StateImplicits extends StateImplicits1 {
  implicit def StateMemberZero[A]: Member.Aux[State[A, ?], State[A, ?] |: NoEffect, NoEffect] = {
    type T[X] = State[A, X]
    Member.zero[T]
  }
  
  implicit def StateMemberFirst[R <: Effects, A]: Member.Aux[State[A, ?], State[A, ?] |: R, R] = {
    type T[X] = State[A, X]
    Member.first[T, R]
  }

  implicit def TaggedStateMemberZero[Tg, A]: Member.Aux[({type l[X] = State[A, X] @@ Tg})#l, ({type l[X] = State[A, X] @@ Tg})#l |: NoEffect, NoEffect] = {
    type T[X] = State[A, X] @@ Tg
    Member.zero[T]
  }

  implicit def TaggedStateMemberFirst[R <: Effects, Tg, A]: Member.Aux[({type l[X] = State[A, X] @@ Tg})#l, ({type l[X] = State[A, X] @@ Tg})#l |: R, R] = {
    type T[X] = State[A, X] @@ Tg
    Member.first[T, R]
  }

}
trait StateImplicits1 {
  implicit def StateMemberSuccessor[O[_], R <: Effects, U <: Effects, A](implicit m: Member.Aux[State[A, ?], R, U]): Member.Aux[State[A, ?], O |: R, O |: U] = {
    type T[X] = State[A, X]
    Member.successor[T, O, R, U]
  }

  implicit def TaggedStateMemberSuccessor[O[_], R <: Effects, U <: Effects, Tg, A](implicit m: Member.Aux[({type l[X] = State[A, X] @@ Tg})#l, R, U]): Member.Aux[({type l[X] = State[A, X] @@ Tg})#l, O |: R, O |: U] = {
    type T[X] = State[A, X] @@ Tg
    Member.successor[T, O, R, U]
  }

}

object StateImplicits extends StateImplicits

