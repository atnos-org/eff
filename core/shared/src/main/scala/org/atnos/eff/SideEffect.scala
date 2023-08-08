package org.atnos.eff

import cats._

trait SideEffect[T[_]] {
  def apply[X](tx: T[X]): X
  def applicative[X, Tr[_]: Traverse](ms: Tr[T[X]]): Tr[X]
}
