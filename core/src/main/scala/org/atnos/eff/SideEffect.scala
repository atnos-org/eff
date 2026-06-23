package org.atnos.eff

import cats.*

trait SideEffect[T[_]] {
  def apply[X](tx: T[X]): X
  def applicative[X, Tr[_]: Traverse](ms: Tr[T[X]]): Tr[X]
}
