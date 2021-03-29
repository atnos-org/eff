package org.atnos.eff.syntax.addon

import _root_.doobie.Transactor
import _root_.doobie.free.connection.ConnectionIO
import org.atnos.eff.addon.doobie._
import org.atnos.eff.{Eff, _}
import cats.effect.MonadCancel

trait doobie {

  implicit final def toDoobieConnectionIOOps[R, A](e: Eff[R, A]): DoobieConnectionIOOps[R, A] =
    new DoobieConnectionIOOps[R, A](e)

}

final class DoobieConnectionIOOps[R, A](private val e: Eff[R, A]) extends AnyVal {
  def runConnectionIO[F[_], U, E, B](t: Transactor[F])(
    implicit mc: Member.Aux[ConnectionIO, R, U],
    mf: MemberInOut[F, U],
    me: MonadCancel[F, Throwable]): Eff[U, A] = {
    DoobieConnectionIOInterpretation.runConnectionIO[R, U, F, E, A, B](e)(t)
  }
}

object doobie extends doobie
