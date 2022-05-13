package org.atnos.eff.addon.doobie

import java.sql.Connection
import _root_.doobie.Transactor
import _root_.doobie.free.connection.ConnectionIO
import cats.effect.Bracket
import cats.syntax.all._
import cats.~>
import org.atnos.eff._
import org.atnos.eff.all._

trait DoobieConnectionIOTypes {
  type _connectionIO[R] = ConnectionIO |= R
  type _ConnectionIO[R] = ConnectionIO <= R
}

trait DoobieConnectionIOCreation extends DoobieConnectionIOTypes {
  final def fromConnectionIO[R: _connectionIO, A](a: ConnectionIO[A]): Eff[R, A] =
    send[ConnectionIO, R, A](a)
}

trait DoobieConnectionIOInterpretation extends DoobieConnectionIOTypes {

  def runConnectionIO[R, U, F[_], E, A, B](
    e: Eff[R, A]
  )(t: Transactor[F])(implicit mc: Member.Aux[ConnectionIO, R, U], mf: F /= U, me: Bracket[F, Throwable]): Eff[U, A] = {

    def getConnection: Eff[U, Connection] =
      send[F, U, Connection](t.connect(t.kernel).allocated.map(_._1))

    def runEffect(connection: Connection): Eff[U, A] =
      interpret.translate(e)(new Translate[ConnectionIO, U] {
        def apply[X](c: ConnectionIO[X]): Eff[U, X] = {
          send[F, U, X](c.foldMap(t.interpret).run(connection))
        }
      })

    def interceptErrors[Y](effect: Eff[U, Y])(oops: F[Unit]): Eff[U, Y] =
      interpret.interceptNat(effect)(new F ~> F {
        def apply[X](f: F[X]): F[X] =
          f.handleErrorWith((err: Throwable) => oops *> me.raiseError[X](err))
      })

    getConnection.flatMap { connection =>
      lazy val always: F[Unit] =
        t.strategy.always.foldMap(t.interpret).run(connection)

      lazy val oops: F[Unit] =
        t.strategy.oops.foldMap(t.interpret).run(connection)

      val before: Eff[U, Unit] =
        send(t.strategy.before.foldMap(t.interpret).run(connection))

      val after: Eff[U, Unit] =
        send(t.strategy.after.foldMap(t.interpret).run(connection))

      interceptErrors(before >> runEffect(connection) << after)(oops).addLast(send(always))
    }
  }
}

object DoobieConnectionIOCreation extends DoobieConnectionIOCreation

object DoobieConnectionIOInterpretation extends DoobieConnectionIOInterpretation

trait DoobieConnectionIOEffect extends DoobieConnectionIOCreation with DoobieConnectionIOInterpretation

object DoobieConnectionIOEffect extends DoobieConnectionIOEffect
