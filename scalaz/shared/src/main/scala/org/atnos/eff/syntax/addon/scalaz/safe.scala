package org.atnos.eff
package syntax.addon.scalaz

import scalaz.*

object safe extends org.atnos.eff.syntax.safe with safe

trait safe {

  given scalazSafeExtension: AnyRef with {

    extension [R, A](e: Eff[R, A]) {

      def runSafeDisjunction[U](using Member.Aux[Safe, R, U]): Eff[U, (Throwable \/ A, List[Throwable])] =
        addon.scalaz.safe.runSafeDisjunction(e)

      def execSafeDisjunction[U](using Member.Aux[Safe, R, U]): Eff[U, Throwable \/ A] =
        addon.scalaz.safe.execSafeDisjunction(e)

      def attemptSafeDisjunction(using Safe /= R): Eff[R, (Throwable \/ A, List[Throwable])] =
        addon.scalaz.safe.attemptSafeDisjunction(e)

      def attemptDisjunction(using MemberInOut[Safe, R]): Eff[R, Throwable \/ A] =
        addon.scalaz.safe.attemptDisjunction(e)

    }
  }
}
