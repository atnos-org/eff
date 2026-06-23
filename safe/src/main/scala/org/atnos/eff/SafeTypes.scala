package org.atnos.eff

trait SafeTypes {

  type _Safe[R] = Safe <= R
  type _safe[R] = Safe |= R
}
