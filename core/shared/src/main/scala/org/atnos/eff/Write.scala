package org.atnos.eff

trait Write[T[_], O] {
  def apply[X](tx: T[X]): O
}

object Write extends WriteCompanion
