package org.atnos.eff.syntax.addon.doobie

import cats.{Monad, MonadError}
import doobie.free.connection.ConnectionIO
import doobie.imports.Transactor
import fs2.util.Catchable
import org.atnos.eff._
import org.atnos.eff.addon.doobie._

trait connectionio {

  implicit final def toDoobieConnectionIOOps[R, A](e: Eff[R, A]): DoobieConnectionIOOps[R, A] =
    new DoobieConnectionIOOps[R, A](e)

}

object connectionio extends connectionio

final class DoobieConnectionIOOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def runConnectionIO[F[_]: Monad: Catchable, E, B](
      t: Transactor[F, B])(implicit ev1: Member.Aux[ConnectionIO, R, Fx.fx1[F]], ev2: MonadError[F, E]): F[A] = {
    DoobieConnectionIOInterpretation.runConnectionIO[R, F, E, A, B](e)(t)
  }
}
