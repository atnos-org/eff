package org.atnos.eff.syntax

import cats.Monoid
import cats.data._
import org.atnos.eff._

object state extends state

trait state {

  implicit class StateEffectOps[R, A](e: Eff[R, A]) {

    def runState[S](s: S)(implicit member: Member[State[S, *], R]): Eff[member.Out, (A, S)] =
      StateInterpretation.runState(s)(e)(member.aux)

    def runStateU[S, U](s: S)(implicit member: Member.Aux[State[S, *], R, U]): Eff[U, (A, S)] =
      StateInterpretation.runState(s)(e)(member)

    def runStateZero[S: Monoid](implicit member: Member[State[S, *], R]): Eff[member.Out, (A, S)] =
      StateInterpretation.runStateZero(e)(Monoid[S], member.aux)

    def evalState[S](s: S)(implicit member: Member[State[S, *], R]): Eff[member.Out, A] =
      StateInterpretation.evalState(s)(e)(member.aux)

    def evalStateU[S, U](s: S)(implicit member: Member.Aux[State[S, *], R, U]): Eff[U, A] =
      StateInterpretation.evalState(s)(e)(member.aux)

    def evalStateZero[S: Monoid](implicit member: Member[State[S, *], R]): Eff[member.Out, A] =
      StateInterpretation.evalStateZero(e)(Monoid[S], member.aux)

    def evalStateZeroU[S: Monoid, U](implicit member: Member.Aux[State[S, *], R, U]): Eff[U, A] =
      StateInterpretation.evalStateZero(e)(Monoid[S], member.aux)

    def execState[S](s: S)(implicit member: Member[State[S, *], R]): Eff[member.Out, S] =
      StateInterpretation.execState(s)(e)(member.aux)

    def execStateU[S, U](s: S)(implicit member: Member.Aux[State[S, *], R, U]): Eff[U, S] =
      StateInterpretation.execState(s)(e)(member.aux)

    def execStateZero[S: Monoid](implicit member: Member[State[S, *], R]): Eff[member.Out, S] =
      StateInterpretation.execStateZero(e)(Monoid[S], member.aux)

    def execStateZeroU[S: Monoid, U](implicit member: Member.Aux[State[S, *], R, U]): Eff[U, S] =
      StateInterpretation.execStateZero(e)(Monoid[S], member.aux)

    def lensState[BR, U, T, S](getter: S => T, setter: (S, T) => S)(implicit
      m1: Member.Aux[State[T, *], R, U],
      m2: Member.Aux[State[S, *], BR, U]
    ): Eff[BR, A] =
      StateInterpretation.lensState[R, BR, U, T, S, A](e, getter, setter)

    def intoState[BR, U1, T, S, U2](getter: S => T, setter: (S, T) => S)(implicit
      m1: Member.Aux[State[T, *], R, U1],
      m2: Member.Aux[State[S, *], BR, U2],
      into: IntoPoly[U1, U2]
    ): Eff[BR, A] =
      StateInterpretation.intoState[R, BR, U1, U2, T, S, A](e, getter, setter)

    def localState[S](modify: S => S)(implicit s: State[S, *] /= R): Eff[R, A] =
      StateInterpretation.localState[R, S, A](e)(modify)

  }

}
