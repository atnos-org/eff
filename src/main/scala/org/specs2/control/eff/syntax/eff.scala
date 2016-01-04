package org.specs2.control.eff
package syntax

import Eff._
import Member._

/**
 * Operations of Eff[R, A] values
 */
object eff {

  implicit class EffOps[R <: Effects, A](e: Eff[R, A]) {
    def into[U](implicit f: IntoPoly[R, U, A]): Eff[U, A] =
      Eff.effInto(e)(f)

    def mapM[M[_]](fx: Mapper[M])(implicit m: M <= R): Eff[R, A] =
      Eff.mapM(e, fx)

    def runM[M[_]](runner: Runner[M, R, A])(implicit m: M <= R): Eff[R, A] =
      Eff.runM(e, runner)
  }

}
