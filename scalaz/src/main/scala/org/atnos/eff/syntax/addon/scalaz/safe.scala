package org.atnos.eff
package syntax.addon.scalaz

import scalaz._

object safe extends org.atnos.eff.syntax.safe with safe

trait safe {

  implicit def toSafeEffectScalazOps[R, A](e: Eff[R, A]): SafeEffectScalazOps[R, A] = new SafeEffectScalazOps[R, A](e)

}

final class SafeEffectScalazOps[R, A](private val e: Eff[R, A]) extends AnyVal {

  def runSafeDisjunction[U](implicit m: Member.Aux[Safe, R, U]): Eff[U, (Throwable \/ A, List[Throwable])] =
    addon.scalaz.safe.runSafeDisjunction(e)

  def execSafeDisjunction[U](implicit m: Member.Aux[Safe, R, U]): Eff[U, Throwable \/ A] =
    addon.scalaz.safe.execSafeDisjunction(e)

  def attemptSafeDisjunction(implicit m: Safe /= R): Eff[R, (Throwable \/ A, List[Throwable])] =
    addon.scalaz.safe.attemptSafeDisjunction(e)

  def attemptDisjunction(implicit member: MemberInOut[Safe, R]): Eff[R, Throwable \/ A] =
    addon.scalaz.safe.attemptDisjunction(e)

}
