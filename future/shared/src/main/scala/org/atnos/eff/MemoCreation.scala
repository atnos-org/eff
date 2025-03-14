package org.atnos.eff

import org.atnos.eff.Eff.*

trait MemoCreation extends MemoTypes {

  def memoize[R: _memo, A](key: AnyRef, a: => A): Eff[R, A] =
    send[Memoized, R, A](Store(key, () => a))

  def getCache[R: _memo]: Eff[R, Cache] =
    send[Memoized, R, Cache](GetCache())

}
