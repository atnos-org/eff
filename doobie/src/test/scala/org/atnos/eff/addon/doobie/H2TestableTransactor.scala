package org.atnos.eff.addon.doobie

import java.util.concurrent.atomic.AtomicInteger

import cats.Monad
import cats.implicits._
import doobie.free.KleisliInterpreter
import doobie.free.connection.{close, delay, commit, rollback, setAutoCommit}
import doobie.util.transactor.{Strategy, Transactor}
import fs2.util.{Catchable, Suspendable}
import org.h2.jdbcx.JdbcConnectionPool

object H2TestableTransactor {

  final class Counters protected[H2TestableTransactor] (val connectionA: AtomicInteger = new AtomicInteger(0),
                                                        val beforeA: AtomicInteger = new AtomicInteger(0),
                                                        val afterA: AtomicInteger = new AtomicInteger(0),
                                                        val oopsA: AtomicInteger = new AtomicInteger(0),
                                                        val alwaysA: AtomicInteger = new AtomicInteger(0)) {
    def connection(): Int = connectionA.get
    def before(): Int     = beforeA.get
    def after(): Int      = afterA.get
    def oops(): Int       = oopsA.get
    def always(): Int     = alwaysA.get
  }

  def create[M[_]](url: String, user: String, pass: String)(
      implicit ev0: Monad[M],
      ev1: Catchable[M],
      ev2: Suspendable[M]): (Transactor[M, JdbcConnectionPool], Counters) = {
    val pool = JdbcConnectionPool.create(url, user, pass)

    val c = new Counters()

    val t = Transactor(
      kernel = pool,
      connect = (a: JdbcConnectionPool) => ev2.delay(a.getConnection) <* ev2.pure(c.connectionA.incrementAndGet()),
      KleisliInterpreter[M](ev0, implicitly, implicitly).ConnectionInterpreter,
      Strategy(
        before = setAutoCommit(false) <* delay(c.beforeA.incrementAndGet()),
        after = commit <* delay(c.afterA.incrementAndGet()),
        oops = rollback <* delay(c.oopsA.incrementAndGet()),
        always = close <* delay(c.alwaysA.incrementAndGet())
      )
    )

    (t, c)
  }

}
