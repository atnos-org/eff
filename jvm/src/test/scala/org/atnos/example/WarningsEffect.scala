package org.atnos
package example

import org.atnos.eff._
import cats.data._

object WarningsEffect {

  type Warnings[A] = Writer[String, A]

  /** warn the user about something that is probably wrong on his side, this is not a specs2 bug */
  def warn[R](message: String)(implicit m: MemberIn[Warnings, R]): Eff[R, Unit] =
    WriterEffect.tell(message)

  /**
   * This interpreter cumulates warnings
   */
  def runWarnings[R, U, A](w: Eff[R, A])(implicit m: Member.Aux[Warnings, R, U]): Eff[U, (A, List[String])] =
    WriterEffect.runWriter(w)

}
