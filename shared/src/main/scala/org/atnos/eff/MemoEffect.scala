package org.atnos.eff

import Eff._
import cats.Eval
import MemoEffect._

/**
 * Memoization effect
 *
 * Memoize a computation for a given key
 *
 * This effect is backed-up by a cache which can be implemented with
 * many different libraries. See Cache.scala for 2 default implementations:
 *
 *  - one concurrent hashmap (meaning an unbounded cache)
 *  - one concurrent hashmap with weak references (to evict entries based on garbage collection)
 *
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

  def memoize[R :_memo, K <: AnyRef, A](key: K, a: =>A): Eff[R, A] =
    send[Memoized, R, A](Store(key, () => a))

  def getCache[R :_memo, K <: AnyRef]: Eff[R, Cache[K]] =
    send[Memoized, R, Cache[K]](GetCache())

}

trait MemoInterpretation extends MemoTypes {


  def runMemo[R, U, A](cache: Cache[AnyRef], effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], eval: Eval |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized  , U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => EvalEffect.delay[U, X](cache.memo(key, value()))
          case GetCache()        => EvalEffect.delay[U, X](cache)
        }
    })
  }

  def runAsyncMemo[R, U, A](cache: Cache[AnyRef], effect: Eff[R, A])(implicit m: Member.Aux[Memoized, R, U], async: Async |= U): Eff[U, A] = {
    interpret.translate(effect)(new Translate[Memoized, U] {
      def apply[X](mx: Memoized[X]): Eff[U, X] =
        mx match {
          case Store(key, value) => AsyncEffect.asyncDelay(cache.memo(key, value()))
          case GetCache()        => AsyncEffect.asyncDelay(cache)
        }
    })
  }

}

object MemoInterpretation extends MemoInterpretation

sealed trait Memoized[A]

case class Store[K <: AnyRef, A](key: K, a: () => A) extends Memoized[A]
case class GetCache[K <: AnyRef]()                   extends Memoized[Cache[K]]
