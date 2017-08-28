package org.atnos.eff.addon.doobie

import org.specs2.Specification
import _root_.doobie.imports._
import fs2._
import fs2.interop.cats._
import org.atnos.eff.{Eff, Fx}
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.addon.doobie._
import org.specs2.matcher.{ThrownExpectations, ValueCheck}
import doobie.free.connection.delay


class DoobieConnectionIOEffectSpec extends Specification with ThrownExpectations { def is = sequential ^ s2"""
  ConnectionIO effects can be converted to Task-like effects $t1

  Failures are properly handled
    in programs                 $t2
    in strategy.before          $t3
    in strategy.after           $t4
    in strategy.oops            $t5
    in strategy.always          $t6
"""

  type Stack = Fx.fx2[ConnectionIO, Task]

  def t1 = {
    val p: Eff[Stack, Int] = for {
      a <- Task.delay(1).send[Stack]
      b <- queryTable(2).send[Stack]
      c <- Task.delay(3).send[Stack]
      d <- queryTable(4).send[Stack]
    } yield a + b + c + d

    val (xa, c) = H2TestableTransactor.create[Task]()

    p.runConnectionIO(xa).detach.unsafeRun must_== 10
    c.calls must_== List("connection", "before", "after", "always")
  }

  def t2 = {
    val p: Eff[Stack, Int] = for {
      a <- Task.delay(1).send[Stack]
      b <- queryTable(2).send[Stack]
      c <- Task.delay[Int](throw new Error("c failed")).send[Stack]
    } yield a + b + c

    val (xa, c) = H2TestableTransactor.create[Task]()

    p.runConnectionIO(xa).detach.unsafeAttemptRun must beLeft(withExceptionMessage("c failed"))
    c.calls must_== List("connection", "before", "oops", "always")
  }

  def t3 = {
    val properProgram: Eff[Stack, Int] = for {
      a <- Task.delay(1).send[Stack]
      b <- queryTable(2).send[Stack]
    } yield a + b

    val (xa, c) = H2TestableTransactor.create[Task](before = delay(throw new Error("before failed")))

    properProgram.runConnectionIO(xa).detach.unsafeAttemptRun must beLeft(withExceptionMessage("before failed"))
    c.calls must_== List("connection", "oops", "always")
  }

  def t4 = {
    val properProgram: Eff[Stack, Int] = for {
      a <- Task.delay(1).send[Stack]
      b <- queryTable(2).send[Stack]
    } yield a + b

    val (xa, c) = H2TestableTransactor.create[Task](after = delay(throw new Error("after failed")))

    properProgram.runConnectionIO(xa).detach.unsafeAttemptRun must beLeft(withExceptionMessage("after failed"))
    c.calls must_== List("connection", "before", "oops", "always")
  }

  def t5 = {
    val erroneousProgram: Eff[Stack, Int] =
      Task.delay[Int](throw new Error("program failed")).send[Stack]

    val (xa, c) = H2TestableTransactor.create[Task](oops = delay(throw new Error("oops failed")))

    erroneousProgram.runConnectionIO(xa).detach.unsafeAttemptRun must beLeft(withExceptionMessage("oops failed"))
    c.calls must_== List("connection", "before", "always")
  }

  def t6 = {
    val properProgram: Eff[Stack, Int] = Task.delay(1).send[Stack]

    val (xa, c) = H2TestableTransactor.create[Task](always = delay(throw new Error("always failed")))

    properProgram.runConnectionIO(xa).detach.unsafeAttemptRun must beLeft(withExceptionMessage("always failed"))
    c.calls must_== List("connection", "before", "after")
  }

  /**
   * HELPERS
   */

  def queryTable(i: Int): ConnectionIO[Int] = sql"select $i".query[Int].unique

  def withExceptionMessage(msg: String): ValueCheck[Throwable] = { err: Throwable =>
    err.getMessage must_== msg
  }
}
