package org.atnos.benchmark

import org.scalameter.api._
import org.atnos.eff._
import EvalEffect._
import Eff._
import cats.implicits._
import cats.Eval
import org.scalameter.picklers.Implicits._

object EffBenchmark extends Bench.OfflineReport {
  type E = Fx.fx1[Eval]

  val sizes = Gen.enumeration("size")(10, 100, 1000, 10000, 100000)

  val lists = for {
    size <- sizes
  } yield (0 until size).toList

  def simpleSend[R, V](v: =>V)(implicit m: Member[Eval, R]) =
    delay(v)


  performance of "send" in {
    measure method "simple send" in {
      using(lists) in { list =>
        run(runEval(list.traverse(a => simpleSend[E, Int](a))))
      }
    }
    measure method "optimised send" in {
      using(lists) in { list =>
        run(runEval(list.traverse(a => delay[E, Int](a))))
      }
    }
  }

}

