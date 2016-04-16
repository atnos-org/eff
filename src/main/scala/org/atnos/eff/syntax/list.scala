package org.atnos.eff.syntax

import org.atnos.eff._

object list extends list

trait list {

  implicit class ListEffectOps[R <: Effects, A](e: Eff[R, A]) {

    def runList[U <: Effects](implicit member: Member.Aux[List, R, U]): Eff[U, List[A]] =
      ListInterpretation.runList(e)

  }

}

