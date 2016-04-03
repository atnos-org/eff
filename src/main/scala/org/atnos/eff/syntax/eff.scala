package org.atnos.eff
package syntax

import Eff._
import Member._
import cats.arrow.NaturalTransformation

/**
 * Operations of Eff[R, A] values
 */
object eff {

  implicit class EffOps[R <: Effects, A](e: Eff[R, A]) {
    def into[U](implicit f: IntoPoly[R, U, A]): Eff[U, A] =
      Eff.effInto(e)(f)

    def transform[M[_], N[_]](t: NaturalTransformation[M, N])(implicit m: M <= R, n: N <= R): Eff[R, A] =
      Eff.transform(e, t)(m, n)

    def runM[M[_]](runner: Runner[M, R, A])(implicit m: M <= R): Eff[R, A] =
      Eff.runM(e, runner)
  }

}
