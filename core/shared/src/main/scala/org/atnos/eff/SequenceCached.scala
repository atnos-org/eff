package org.atnos.eff

/**
 * type class for effects which can be cached
 * in a SequenceCache
 */
trait SequenceCached[M[_]] {
  def get[X](cache: Cache, key: AnyRef): M[Option[X]]
  def apply[X](cache: Cache, key: AnyRef, sequenceKey: Int, tx: => M[X]): M[X]
  def reset(cache: Cache, key: AnyRef): M[Unit]
}
