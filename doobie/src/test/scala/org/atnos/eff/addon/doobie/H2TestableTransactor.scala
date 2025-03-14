package org.atnos.eff.addon.doobie

import cats.effect.*
import cats.syntax.all.*
import doobie.free.connection.ConnectionIO
import doobie.free.connection.close
import doobie.free.connection.commit
import doobie.free.connection.delay
import doobie.free.connection.rollback
import doobie.free.connection.setAutoCommit
import doobie.util.transactor.Strategy
import doobie.util.transactor.Transactor
import java.util.concurrent.Executors
import org.h2.jdbcx.JdbcConnectionPool
import scala.concurrent.ExecutionContext

object H2TestableTransactor {

  final class OpHistory {
    var calls: List[String] = List.empty[String]

    def registerConnection(): Unit = calls :+= "connection"
    def registerBefore(): Unit = calls :+= "before"
    def registerAfter(): Unit = calls :+= "after"
    def incrementOops(): Unit = calls :+= "oops"
    def registerAlways(): Unit = calls :+= "always"
  }

  def create[M[_]: ContextShift](
    url: String = "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",
    user: String = "sa",
    pass: String = "",
    before: ConnectionIO[Unit] = setAutoCommit(false),
    after: ConnectionIO[Unit] = commit,
    oops: ConnectionIO[Unit] = rollback,
    always: ConnectionIO[Unit] = close
  )(implicit async: Async[M]): (Transactor[M], OpHistory) = {

    val pool = JdbcConnectionPool.create(url, user, pass)

    val c = new OpHistory()

    val ec = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool)

    val blocker = Blocker.liftExecutionContext(ec)

    val pre = Transactor.fromDataSource.apply(pool, ec, blocker)

    val t = pre.copy(
      connect0 = con => pre.connect(con).evalTap(async.pure(_) <* async.pure(c.registerConnection())),
      strategy0 = Strategy(
        before = before.flatMap(a => delay(c.registerBefore()).map(_ => a)),
        after = after.flatMap(a => delay(c.registerAfter()).map(_ => a)),
        oops = oops.flatMap(a => delay(c.incrementOops()).map(_ => a)),
        always = always.flatMap(a => delay(c.registerAlways()).map(_ => a))
      )
    )

    (t, c)
  }

}
