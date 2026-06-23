package org.atnos.eff

trait MemoTypes {
  type _Memo[R] = Memoized <= R
  type _memo[R] = Memoized |= R
}

object MemoTypes extends MemoTypes
