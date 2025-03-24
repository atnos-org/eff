package org.atnos.eff

sealed abstract class Memoized[A]

case class Store[A](key: AnyRef, a: () => A) extends Memoized[A]
case class GetCache() extends Memoized[Cache]
