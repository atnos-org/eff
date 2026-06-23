package org.atnos.eff.syntax

import cats.*
import org.atnos.eff.*

object memo extends memo

trait memo {

  given memoExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {
      def runMemo[U](cache: Cache)(using member: Member.Aux[Memoized, R, U], eval: Eval |= U): Eff[U, A] =
        MemoEffect.runMemo(cache)(e)(using member, eval)

      def runFutureMemo[U](cache: Cache)(using memMember: Member.Aux[Memoized, R, U], futMember: TimedFuture |= U): Eff[U, A] =
        MemoEffect.runFutureMemo(cache)(e)(using memMember, futMember)
    }
  }

}
