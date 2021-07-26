package org.atnos.eff

import cats.Eval

case class ConcurrentWeakIdentityHashMapCache(
  map: ConcurrentWeakIdentityHashMap[AnyRef, Eval[Any]] = new ConcurrentWeakIdentityHashMap[AnyRef, Eval[Any]]) extends Cache {

  type C = Cache

  def memo[V](key: AnyRef, value: =>V): V = {
    lazy val v = value
    if (map.putIfAbsent(key, Eval.later(v).memoize) == null) v
    else map.get(key).value.asInstanceOf[V]
  }

  def put[V](key: AnyRef, value: V): V =
    map.put(key, Eval.now(value)).value.asInstanceOf[V]

  def get[V](key: AnyRef): Option[V] =
    Option(map.get(key)).map(_.value.asInstanceOf[V])

  def reset(key: AnyRef) = {
    map.remove(key.hashCode)
    this
  }

}
