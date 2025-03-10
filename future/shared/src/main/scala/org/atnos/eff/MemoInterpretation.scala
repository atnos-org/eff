package org.atnos.eff

import cats.Eval

trait MemoInterpretation extends MemoTypes {

  def runMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(using Member.Aux[Memoized, R, U], Eval |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => EvalEffect.delay[U, X](cache.memo(key, value()))
          case GetCache() => EvalEffect.delay[U, X](cache)
        }
    })
  }

  def runFutureMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(using Member.Aux[Memoized, R, U], TimedFuture |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => FutureEffect.futureDelay(cache.memo(key, value()))
          case GetCache() => FutureEffect.futureDelay(cache)
        }
    })
  }
}

object MemoInterpretation extends MemoInterpretation
