package org.atnos.eff

import java.util.concurrent._
import cats.Eval

/**
 * This cache is used to memoize values for the Memoized effect
 */
trait Cache {

  type C <: Cache

  /**
   * store a value for a given key, subsequent calls to memo will return the same value
   */
  def memo[V](key: AnyRef, value: =>V): V

  /**
   * put a value for a given key and override the previous value if present
   */
  def put[V](key: AnyRef, value: V): V

  /**
   * get a value for a given key
   */
  def get[V](key: AnyRef): Option[V]

  /**
   * remove the given key
   */
  def reset(key: AnyRef): C

}

/**
 * type class for effects which can be cached
 * in a SequenceCache
 */
trait SequenceCached[M[_]] {
  def get[X](cache: Cache, key: AnyRef): M[Option[X]]
  def apply[X](cache: Cache, key: AnyRef, sequenceKey: Int, tx: =>M[X]): M[X]
  def reset(cache: Cache, key: AnyRef): M[Unit]
}

case class ConcurrentHashMapCache(map: ConcurrentHashMap[AnyRef, Eval[Any]] = new ConcurrentHashMap[AnyRef, Eval[Any]]) extends Cache {

  type C = Cache

  def memo[V](key: AnyRef, value: =>V): V = {
    lazy val v = value
    if (map.putIfAbsent(key, Eval.later(v).memoize) == null) v
    else map.get(key).value.asInstanceOf[V]
  }

  def put[V](key: AnyRef, value: V): V = {
    val v = Eval.now(value)
    map.put(key, v)
    Option(map.get(key)).getOrElse(v).value.asInstanceOf[V]
  }

  def get[V](key: AnyRef): Option[V] =
    Option(map.get(key)).map(_.value.asInstanceOf[V])

  def reset(key: AnyRef) = {
    map.remove(key)
    this
  }
}
