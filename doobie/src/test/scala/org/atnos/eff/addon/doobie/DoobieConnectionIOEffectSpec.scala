package org.atnos.eff.addon.doobie

import org.specs2.Specification
import doobie.imports._
import fs2._
import fs2.interop.cats._
import org.atnos.eff.{Eff, Fx}
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.addon.doobie.connectionio._
import org.specs2.matcher.ThrownExpectations

class DoobieConnectionIOEffectSpec extends Specification with ThrownExpectations { def is = sequential ^ s2"""

 ConnectionIO effects can be converted to Task-like effects           $t1
 Failures in programs with ConnectionIO effects are properly handled  $t2

"""

  def t1 = {

    type Stack = Fx.fx2[ConnectionIO, Task]

    def select(i: Int): ConnectionIO[Int] = sql"select $i".query[Int].unique

    val p: Eff[Stack, Int] = for {
      a <- Task.delay(1).send[Stack]
      b <- select(2).send[Stack]
      c <- Task.delay(3).send[Stack]
      d <- select(4).send[Stack]
    } yield a + b + c + d

    val (xa, c) = H2TestableTransactor.create[Task]("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "")

    val resTask: Task[Int] = p.runConnectionIO(xa)

    resTask.unsafeRun() must_== 10
    c.connection() must_== 1
    c.before() must_== 1
    c.after() must_== 1
    c.oops() must_== 0
    c.always() must_== 1
  }

  def t2 = {

    type Stack = Fx.fx2[ConnectionIO, Task]

    def select(i: Int): ConnectionIO[Int] = sql"select $i".query[Int].unique

    val p: Eff[Stack, Int] = for {
      a <- Task.delay(1).send[Stack]
      b <- select(2).send[Stack]
      c <- Task.delay[Int](throw new Error("failed")).send[Stack]
    } yield a + b + c

    val (xa, c) = H2TestableTransactor.create[Task]("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", "sa", "")

    val resTask: Task[Int] = p.runConnectionIO(xa)

    resTask.unsafeAttemptRun() must beLeft
    c.connection() must_== 1
    c.before() must_== 1
    c.after() must_== 0
    c.oops() must_== 1
    c.always() must_== 1
  }
}
