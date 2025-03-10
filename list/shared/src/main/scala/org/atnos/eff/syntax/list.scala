package org.atnos.eff.syntax

import org.atnos.eff.*

object list extends list

trait list {

  given listExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {
      def runList(using member: Member[List, R]): Eff[member.Out, List[A]] =
        ListInterpretation.runList(e)(using member.aux)
    }
  }

}
