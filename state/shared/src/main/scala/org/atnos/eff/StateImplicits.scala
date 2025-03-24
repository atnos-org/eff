package org.atnos.eff

import cats.*
import cats.data.*

trait StateImplicits {

  given stateMemberInToReaderMemberIn[E, S](using m: MemberIn[State[S, *], E]): MemberIn[Reader[S, *], E] =
    m.transform(using readerToStateNat)

  given stateMemberInLens[E, S, T](using m: MemberIn[State[S, *], E], get: S => T, set: T => S => S): MemberIn[State[T, *], E] =
    m.transform(using via(get, set))

  def readerToStateNat[S1]: Reader[S1, *] ~> State[S1, *] = new (Reader[S1, *] ~> State[S1, *]) {
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
