package org.atnos.eff

/** support trait for folding values while possibly keeping some internal state */
trait RightFold[A, B] {
  type S
  val init: S
  def fold(a: A, s: S): S
  def finalize(s: S): B
}
