package org.atnos.eff

import Eff._
import cats.Eval

/**
 * Memoization effect
 *
 * Memoize a computation for a given key
 *
 * This effect can be interpreted with a cache implemented with
 * many different libraries. See Cache.scala for 2 default implementations:
 *
 *  - one concurrent hashmap (meaning an unbounded cache)
 *  - one concurrent hashmap with weak references (to evict entries based on garbage collection)
 *
 * You can implement your own version using ScalaCache for example
 */

trait MemoEffect extends
  MemoTypes with
  MemoCreation with
  MemoInterpretation

object MemoEffect extends MemoEffect

trait MemoTypes {
  type _Memo[R] = Memoized <= R
  type _memo[R] = Memoized |= R
}

object MemoTypes extends MemoTypes

trait MemoCreation extends MemoTypes {

  def memoize[R :_memo, A](key: AnyRef, a: =>A): Eff[R, A] =
    send[Memoized, R, A](Store(key, () => a))

  def getCache[R :_memo]: Eff[R, Cache] =
    send[Memoized, R, Cache](GetCache())

}

trait MemoInterpretation extends MemoTypes {

  def runMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], eval: Eval |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized  , U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => EvalEffect.delay[U, X](cache.memo(key, value()))
          case GetCache()        => EvalEffect.delay[U, X](cache)
        }
    })
  }

  def runFutureMemo[R, U, A](cache: Cache)(effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], future: TimedFuture |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => FutureEffect.futureDelay(cache.memo(key, value()))
          case GetCache()        => FutureEffect.futureDelay(cache)
        }
    })
  }
}

object MemoInterpretation extends MemoInterpretation

sealed trait Memoized[A]

case class Store[A](key: AnyRef, a: () => A) extends Memoized[A]
case class GetCache()                        extends Memoized[Cache]
