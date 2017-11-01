package org.atnos.eff.addon.doobie

import cats.effect.Async
import cats.implicits._
import doobie.free.KleisliInterpreter
import doobie.free.connection.{ConnectionIO, close, commit, delay, rollback, setAutoCommit}
import doobie.free.connection.AsyncConnectionIO
import doobie.util.transactor.{Strategy, Transactor}
import org.h2.jdbcx.JdbcConnectionPool

object H2TestableTransactor {

  final class OpHistory {
    var calls: List[String] = List.empty[String]

    def registerConnection(): Unit = calls :+= "connection"
    def registerBefore(): Unit     = calls :+= "before"
    def registerAfter(): Unit      = calls :+= "after"
    def incrementOops(): Unit      = calls :+= "oops"
    def registerAlways(): Unit     = calls :+= "always"
  }

  def create[M[_]](url: String = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
                   user: String = "sa",
                   pass: String = "",
                   before: ConnectionIO[Unit] = setAutoCommit(false),
                   after: ConnectionIO[Unit] = commit,
                   oops: ConnectionIO[Unit] = rollback,
                   always: ConnectionIO[Unit] = close)(
      implicit ev: Async[M]): (Transactor[M], OpHistory) = {
    val pool = JdbcConnectionPool.create(url, user, pass)

    val c = new OpHistory()

    val t = Transactor(
      kernel0 = pool,
      connect0 = (a: JdbcConnectionPool) => ev.delay(a.getConnection) <* ev.pure(c.registerConnection()),
      KleisliInterpreter[M].ConnectionInterpreter,
      Strategy(
        before = before <* delay(c.registerBefore()),
        after = after <* delay(c.registerAfter()),
        oops = oops <* delay(c.incrementOops()),
        always = always <* delay(c.registerAlways())
      )
    )

    (t, c)
  }

}
