package org.atnos.eff

trait Augment[T[_], O[_]] {
  def apply[X](tx: T[X]): O[Unit]
}

object Augment extends AugmentCompanion
