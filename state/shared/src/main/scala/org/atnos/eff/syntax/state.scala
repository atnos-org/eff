package org.atnos.eff.syntax

import cats.Monoid
import cats.data.*
import org.atnos.eff.*

object state extends state

trait state {

  given stateExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {

      def runState[S](s: S)(using member: Member[State[S, *], R]): Eff[member.Out, (A, S)] =
        StateInterpretation.runState(s)(e)(using member.aux)

      def runStateU[S, U](s: S)(using member: Member.Aux[State[S, *], R, U]): Eff[U, (A, S)] =
        StateInterpretation.runState(s)(e)(using member)

      def runStateZero[S: Monoid](using member: Member[State[S, *], R]): Eff[member.Out, (A, S)] =
        StateInterpretation.runStateZero(e)(using Monoid[S], member.aux)

      def evalState[S](s: S)(using member: Member[State[S, *], R]): Eff[member.Out, A] =
        StateInterpretation.evalState(s)(e)(using member.aux)

      def evalStateU[S, U](s: S)(using member: Member.Aux[State[S, *], R, U]): Eff[U, A] =
        StateInterpretation.evalState(s)(e)(using member.aux)

      def evalStateZero[S: Monoid](using member: Member[State[S, *], R]): Eff[member.Out, A] =
        StateInterpretation.evalStateZero(e)(using Monoid[S], member.aux)

      def evalStateZeroU[S: Monoid, U](using member: Member.Aux[State[S, *], R, U]): Eff[U, A] =
        StateInterpretation.evalStateZero(e)(using Monoid[S], member.aux)

      def execState[S](s: S)(using member: Member[State[S, *], R]): Eff[member.Out, S] =
        StateInterpretation.execState(s)(e)(using member.aux)

      def execStateU[S, U](s: S)(using member: Member.Aux[State[S, *], R, U]): Eff[U, S] =
        StateInterpretation.execState(s)(e)(using member.aux)

      def execStateZero[S: Monoid](using member: Member[State[S, *], R]): Eff[member.Out, S] =
        StateInterpretation.execStateZero(e)(using Monoid[S], member.aux)

      def execStateZeroU[S: Monoid, U](using member: Member.Aux[State[S, *], R, U]): Eff[U, S] =
        StateInterpretation.execStateZero(e)(using Monoid[S], member.aux)

      def lensState[BR, U, T, S](getter: S => T, setter: (S, T) => S)(using
        Member.Aux[State[T, *], R, U],
        Member.Aux[State[S, *], BR, U]
      ): Eff[BR, A] =
        StateInterpretation.lensState[R, BR, U, T, S, A](e, getter, setter)

      def intoState[BR, U1, T, S, U2](getter: S => T, setter: (S, T) => S)(using
        Member.Aux[State[T, *], R, U1],
        Member.Aux[State[S, *], BR, U2],
        IntoPoly[U1, U2]
      ): Eff[BR, A] =
        StateInterpretation.intoState[R, BR, U1, U2, T, S, A](e, getter, setter)

      def localState[S](modify: S => S)(using State[S, *] /= R): Eff[R, A] =
        StateInterpretation.localState[R, S, A](e)(modify)

    }
  }

}
