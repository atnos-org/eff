package org.atnos.eff

import cats.Eval
import java.util.concurrent.*

case class ConcurrentHashMapCache(map: ConcurrentHashMap[AnyRef, Eval[Any]] = new ConcurrentHashMap[AnyRef, Eval[Any]]) extends Cache {

  type C = Cache

  def memo[V](key: AnyRef, value: => V): V = {
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

  def reset(key: AnyRef): C = {
    map.remove(key)
    this
  }
}
