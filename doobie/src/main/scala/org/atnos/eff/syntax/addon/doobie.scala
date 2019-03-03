package org.atnos.eff.syntax.addon

import org.atnos.eff.Eff
import cats.MonadError
import org.atnos.eff._
import org.atnos.eff.addon.doobie._
import _root_.doobie.imports.{Transactor, ConnectionIO}


trait doobie {

  implicit final def toDoobieConnectionIOOps[R, A](e: Eff[R, A]): DoobieConnectionIOOps[R, A] =
    new DoobieConnectionIOOps[R, A](e)

}

final class DoobieConnectionIOOps[R, A](private val e: Eff[R, A]) extends AnyVal {
  def runConnectionIO[F[_], U, E, B](t: Transactor[F])(
    implicit mc: Member.Aux[ConnectionIO, R, U],
    mf: MemberInOut[F, U],
    me: MonadError[F, E]): Eff[U, A] = {
    DoobieConnectionIOInterpretation.runConnectionIO[R, U, F, E, A, B](e)(t)
  }
}

object doobie extends doobie
