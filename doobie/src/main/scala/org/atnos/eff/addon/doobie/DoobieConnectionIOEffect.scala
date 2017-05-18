package org.atnos.eff.addon.doobie

import java.sql.Connection

import cats.implicits._
import cats.{Monad, MonadError}
import cats.free.Free
import doobie.free.connection.ConnectionIO
import doobie.imports.Transactor
import fs2.util.Catchable
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

trait DoobieConnectionIOTypes {
  type _connectionIO[R] = ConnectionIO |= R
  type _ConnectionIO[R] = ConnectionIO <= R
}

trait DoobieConnectionIOCreation extends DoobieConnectionIOTypes {
  final def pure[R: _connectionIO, A](a: A): Eff[R, A] =
    send[ConnectionIO, R, A](Free.pure(a))

  final def fromConnectionIO[R: _connectionIO, A](a: ConnectionIO[A]): Eff[R, A] =
    send[ConnectionIO, R, A](a)
}

trait DoobieConnectionIOInterpretation extends DoobieConnectionIOTypes {
  def runConnectionIO[R, F[_]: Monad: Catchable, E, A, B](e: Eff[R, A])(
      t: Transactor[F, B])(implicit m1: Member.Aux[ConnectionIO, R, Fx.fx1[F]], me: MonadError[F, E]): F[A] = {

    type FStack = Fx.fx1[F]

    (for {
      c <- send[F, FStack, Connection](t.connect(t.kernel))
      res <- {

        val eInterpreted: Eff[FStack, A] = {
          interpret.translate(e)(new Translate[ConnectionIO, FStack] {
            override def apply[X](kv: ConnectionIO[X]): Eff[FStack, X] = {
              send[F, FStack, X](kv.foldMap(t.interpret).run(c))
            }
          })
        }

        val beforeAfter: F[A] = for {
          _   <- t.strategy.before.foldMap(t.interpret).run(c)
          res <- eInterpreted.into[FStack].detach(me)
          _   <- t.strategy.after.foldMap(t.interpret).run(c)
        } yield res

        val always = t.strategy.always.foldMap(t.interpret).run(c)
        val oops   = t.strategy.oops.foldMap(t.interpret).run(c)

        val ErrF = MonadError[F, E]

        send[F, FStack, A](
          ErrF.handleErrorWith(beforeAfter)(err => oops *> always *> ErrF.raiseError(err)) <* always
        )
      }
    } yield res).detach
  }
}

object DoobieConnectionIOCreation extends DoobieConnectionIOCreation

object DoobieConnectionIOInterpretation extends DoobieConnectionIOInterpretation

trait DoobieConnectionIOEffect extends DoobieConnectionIOCreation with DoobieConnectionIOInterpretation

object DoobieConnectionIOEffect extends DoobieConnectionIOEffect
