package org.atnos.eff.addon.doobie

import cats.implicits._
import doobie.free.KleisliInterpreter
import doobie.free.connection.{ConnectionIO, close, commit, delay, rollback, setAutoCommit}
import doobie.util.transactor.{Strategy, Transactor}
import cats.effect._
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
                   after:  ConnectionIO[Unit] = commit,
                   oops:   ConnectionIO[Unit] = rollback,
                   always: ConnectionIO[Unit] = close)(
      implicit async: Async[M]): (Transactor[M], OpHistory) = {
    
    val pool = JdbcConnectionPool.create(url, user, pass)

    val c = new OpHistory()

    val t = Transactor(
      kernel0 = pool,
      connect0 = (a: JdbcConnectionPool) => async.delay(a.getConnection) <* async.pure(c.registerConnection()),
      KleisliInterpreter[M].ConnectionInterpreter,
      Strategy(
        before = before.flatMap(a => delay(c.registerBefore()).map(_ => a)),
        after  = after .flatMap(a => delay(c.registerAfter()) .map(_ => a)),
        oops   = oops  .flatMap(a => delay(c.incrementOops()) .map(_ => a)),
        always = always.flatMap(a => delay(c.registerAlways()).map(_ => a))
      )
    )

    (t, c)
  }

}
