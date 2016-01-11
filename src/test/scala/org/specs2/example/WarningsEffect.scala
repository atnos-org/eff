package org.specs2
package example

import org.specs2.control.eff._
import cats.data._
import Tag._

object WarningsEffect {

  trait WarningsTag

  type Warnings[A] = Writer[String, A] @@ WarningsTag

  /** warn the user about something that is probably wrong on his side, this is not a specs2 bug */
  def warn[R](message: String)(implicit m: Member[Warnings, R]): Eff[R, Unit] =
    WriterEffect.tell(message)(Member.untagMember[Writer[String, ?], R, WarningsTag](m))

  /**
   * This interpreter cumulates warnings
   */
  def runWarnings[R <: Effects, U <: Effects, A](w: Eff[R, A])(implicit m: Member.Aux[Warnings, R, U]): Eff[U, (A, List[String])] =
    WriterEffect.runTaggedWriter[R, U, WarningsTag, String, A](w)

}
