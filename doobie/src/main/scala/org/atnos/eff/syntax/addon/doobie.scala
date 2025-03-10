package org.atnos.eff.syntax.addon

import _root_.doobie.Transactor
import _root_.doobie.free.connection.ConnectionIO
import cats.effect.Bracket
import org.atnos.eff.*
import org.atnos.eff.addon.doobie.*

trait doobie {

  given doobieExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {
      def runConnectionIO[F[_], U](
        t: Transactor[F]
      )(using Member.Aux[ConnectionIO, R, U], MemberInOut[F, U], Bracket[F, Throwable]): Eff[U, A] = {
        DoobieConnectionIOInterpretation.runConnectionIO[R, U, F, A](e)(t)
      }
    }
  }

}

object doobie extends doobie
