package org.atnos.eff
package addon.scalaz

import scalaz._
import cats.implicits._

object safe extends safe

trait safe {

  def runSafeDisjunction[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Safe, R, U]): Eff[U, (Throwable \/ A, List[Throwable])] =
    all.runSafe(r).map(_.leftMap(_.fold(\/.left, \/.right)))

  def execSafeDisjunction[R, U, A](r: Eff[R, A])(implicit m: Member.Aux[Safe, R, U]): Eff[U, Throwable \/ A] =
    all.execSafe(r).map(_.fold(\/.left, \/.right))

  def attemptSafeDisjunction[R, A](r: Eff[R, A])(implicit m: Safe /= R): Eff[R, (Throwable \/ A, List[Throwable])] =
    all.attemptSafe(r).map(_.leftMap(_.fold(\/.left, \/.right)))

  def attemptDisjunction[R, A](action: Eff[R, A])(implicit m: Safe /= R): Eff[R, Throwable \/ A] =
    all.attempt(action).map(_.fold(\/.left, \/.right))


}
