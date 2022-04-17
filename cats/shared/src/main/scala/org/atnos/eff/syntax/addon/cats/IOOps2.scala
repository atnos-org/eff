package org.atnos.eff.syntax.addon.cats

import cats.effect.IO
import org.atnos.eff._
import org.atnos.eff.addon.cats.effect.IOEffect
import org.atnos.eff.addon.cats.effect.IOInterpretation

final class IOOps2[R, A](private val e: Eff[R, A]) extends AnyVal {

  def ioAttempt(implicit m: MemberInOut[IO, R]): Eff[R, Throwable Either A] =
    IOEffect.ioAttempt(e)

  def runIoMemo[U](cache: Cache)(implicit m: Member.Aux[Memoized, R, U], task: IO |= U): Eff[U, A] =
    IOEffect.runIoMemo(cache)(e)

  def ioMemo(key: AnyRef, cache: Cache)(implicit task: IO /= R): Eff[R, A] =
    IOInterpretation.ioMemo(key, cache, e)

}
