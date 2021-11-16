package org.atnos.eff

import cats.syntax.all._
import Interpret._
import Eff._
import cats._
import data._

/**
 * Effect for passing state along computations
 *
 * Internally backed up by cats.data.State
 *
 */
trait StateEffect extends
  StateCreation with
  StateInterpretation

object StateEffect extends StateEffect

trait StateCreation {

  /** store a new state value */
  def put[R, S](s: S)(implicit member: State[S, *] |= R): Eff[R, Unit] =
    send[State[S, *], R, Unit](State.set(s))

  /** get the current state value */
  def get[R, S](implicit member: State[S, *] |= R): Eff[R, S] =
    send[State[S, *], R, S](State.get)

  /** get the current state value and map it with a function f */
  def gets[R, S, T](f: S => T)(implicit member: State[S, *] |= R): Eff[R, T] =
    send[State[S, *], R, T](State.inspect(f))

  /** modify the current state value */
  def modify[R, S](f: S => S)(implicit member: State[S, *] |= R): Eff[R, Unit] =
    send[State[S, *], R, Unit](State.modify(f))

}

trait StateImplicits {

  implicit def stateMemberInToReaderMemberIn[E, S](implicit m: MemberIn[State[S, *], E]): MemberIn[Reader[S, *], E] =
    m.transform(readerToStateNat)

  implicit def stateMemberInLens[E, S, T](implicit m: MemberIn[State[S, *], E], get: S => T, set: T => S => S): MemberIn[State[T, *], E] =
    m.transform(via(get, set))

  def readerToStateNat[S1] = new (Reader[S1, *] ~> State[S1, *]) {
    def apply[X](r: Reader[S1, X]): State[S1, X] =
      State((s: S1) => (s, r.run(s)))
  }

  def via[S, T](get: S => T, set: T => S => S): State[T, *] ~> State[S, *] =
    new (State[T, *] ~> State[S, *]) {
      def apply[X](s: State[T, X]) =
        State[S, X] { s1 =>
          val (t, x) = s.run(get(s1)).value
          (set(t)(s1), x)
        }
    }

}

object StateImplicits extends StateImplicits

object StateCreation extends StateCreation

trait StateInterpretation {
  /** run a state effect, with a Monoidal state */
  def evalStateZero[R, U, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[State[S, *], R, U]): Eff[U, A] =
    evalState(Monoid[S].empty)(w)

  /** run a state effect, with an initial value, return only the value */
  def evalState[R, U, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[State[S, *], R, U]): Eff[U, A] =
    runState(initial)(w).map(_._1)

  /** run a state effect, with a monoidal state, return only the state */
  def execStateZero[R, U, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[State[S, *], R, U]): Eff[U, S] =
    execState(Monoid[S].empty)(w)

  /** run a state effect, with an initial value, return only the state */
  def execState[R, U, S, A](initial: S)(w: Eff[R, A])(implicit m: Member.Aux[State[S, *], R, U]): Eff[U, S] =
    runState(initial)(w).map(_._2)

  /** run a state effect, with an initial value */
  def runStateZero[R, U, S : Monoid, A](w: Eff[R, A])(implicit m: Member.Aux[State[S, *], R, U]): Eff[U, (A, S)] =
    runState(Monoid[S].empty)(w)

  /** run a state effect, with an initial value */
  def runState[R, U, S1, A](initial: S1)(w: Eff[R, A])(implicit m: Member.Aux[State[S1, *], R, U]): Eff[U, (A, S1)] =
    runInterpreter[R, U, State[S1, *], A, (A, S1)](w)(new Interpreter[State[S1, *], U, A, (A, S1)] {
      private[this] var s: S1 = initial

      def onPure(a: A): Eff[U, (A, S1)] =
        Eff.pure((a, s))

      def onEffect[X](state: State[S1, X], continuation: Continuation[U, X, (A, S1)]): Eff[U, (A, S1)] = {
        val (s1, x) = state.run(s).value
        s = s1
        Eff.impure(x, continuation)
      }

      def onLastEffect[X](x: State[S1, X], continuation: Continuation[U, X, Unit]): Eff[U, Unit] =
        Eff.pure(())

      def onApplicativeEffect[X, T[_]: Traverse](ms: T[State[S1, X]], continuation: Continuation[U, T[X], (A, S1)]): Eff[U, (A, S1)] = {
        val (s1, xs) = ms.sequence.run(s).value
        s = s1
        Eff.impure(xs, continuation)
      }
    })

  /**
   * Lift a computation over a "small" state (for a subsystem) into
   * a computation over a "bigger" state (for the full application state)
   */
  def lensState[TS, SS, U, T, S, A](state: Eff[TS, A], getter: S => T, setter: (S, T) => S)
                                        (implicit ts: Member.Aux[State[T, *], TS, U],
                                                  ss: Member.Aux[State[S, *], SS, U]): Eff[SS, A] =
    intoState(state, getter, setter)

  /**
   * General lifting of a state effect into another
   * from one stack to another. This will require a type annotation
   */
  def intoState[TS, SS, U1, U2, T, S, A](state: Eff[TS, A], getter: S => T, setter: (S, T) => S)
                                        (implicit ts: Member.Aux[State[T, *], TS, U1],
                                                  ss: Member.Aux[State[S, *], SS, U2],
                                                  into: IntoPoly[U1, U2]): Eff[SS, A] =
    Interpret.transform[TS, SS, U1, U2, State[T, *], State[S, *], A](state, new ~>[State[T, *], State[S, *]] {
      def apply[X](tstate: State[T, X]): State[S, X] =
        State { (s: S) =>
          val (t, x) = tstate.run(getter(s)).value
          (setter(s, t), x)
        }
    })

  /**
   * Update the state value, the stack of the Eff computation stays the same
   */
  def localState[R, S, A](e: Eff[R, A])(modify: S => S)(implicit s: State[S, *] /= R): Eff[R, A] =
    interceptNat(e)(new ~>[State[S, *], State[S, *]] {
      def apply[X](r: State[S, X]): State[S, X] =
        r.modify(modify)
    })

}

object StateInterpretation extends StateInterpretation

