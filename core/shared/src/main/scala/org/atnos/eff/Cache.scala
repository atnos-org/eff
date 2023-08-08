package org.atnos.eff

/**
 * This cache is used to memoize values for the Memoized effect
 */
trait Cache {

  type C <: Cache

  /**
   * store a value for a given key, subsequent calls to memo will return the same value
   */
  def memo[V](key: AnyRef, value: => V): V

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
