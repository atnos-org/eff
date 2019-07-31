package org.atnos.eff.syntax.addon.zio

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2._
import zio.{DefaultRuntime, Task, UIO}

class ZIOEffectSpec extends Specification with ScalaCheck { def is = "zio".title ^ sequential ^ s2"""
 ZIO can work as normal values                           $e1
 ZIO effects can be attemptedEither                      $e2
"""


  object Runtime extends DefaultRuntime

  type S1 = Fx.fx2[UIO, Option]

  def e1 = {
    def action[R :_uio :_option]: Eff[R, Int] = for {
      a <- succeedLazy(10)
      b <- succeedLazy(20)
    } yield a + b

    val zio = action[S1].runOption.runSequential

    Runtime.unsafeRun(zio) must beSome(30)
  }

  type S2 = Fx.fx2[Task, Option]

  def e2 = {
    def action[R :_task :_option]: Eff[R, Int] = for {
      a <- task(10)
      b <- task { boom; 20 }
    } yield a + b

    val zio = action[S2].either.runOption.runSequential
    Runtime.unsafeRun(zio) must beSome(beLeft(boomException))
  }



  /**
   * HELPERS
   */
  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")
}
