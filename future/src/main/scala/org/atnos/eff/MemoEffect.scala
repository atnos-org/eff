package org.atnos.eff

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

trait MemoEffect extends MemoTypes with MemoCreation with MemoInterpretation

object MemoEffect extends MemoEffect
