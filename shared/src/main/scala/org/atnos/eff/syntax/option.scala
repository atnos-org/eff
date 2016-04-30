package org.atnos.eff.syntax

import org.atnos.eff._

object option extends option

trait option {

  implicit class OptionEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runOption[U <: Effects](implicit member: Member.Aux[Option, R, U]): Eff[U, Option[A]] =
      OptionInterpretation.runOption(e)

  }

}
