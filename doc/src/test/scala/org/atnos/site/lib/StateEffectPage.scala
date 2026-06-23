package org.atnos.site
package lib

object StateEffectPage extends UserGuidePage {
  def is = "State".title ^ s2"""

A `State` effect can be seen as the combination of both a `Reader` and a `Writer` with these operations:

 - `get` get the current state

 - `put` set a new state

Let's see an example showing that we can also use tags to track different states at the same time:${snippet {
      import cats.data._
      import org.atnos.eff._, all._, syntax.all.given

      type S1[A] = State[Int, A]
      type S2[A] = State[String, A]

      type S = Fx.fx2[S1, S2]

      val swapVariables: Eff[S, String] = for {
        v1 <- get[S, Int]
        v2 <- get[S, String]
        _ <- put[S, Int](v2.size)
        _ <- put[S, String](v1.toString)
        w1 <- get[S, Int]
        w2 <- get[S, String]
      } yield "initial: " + (v1, v2).toString + ", final: " + (w1, w2).toString

      swapVariables.evalState(10).evalState("hello").run
    }.eval}

In the example above we have used an `eval` method to get the `A` in `Eff[R, A]` but it is also possible to get both the
 value and the state with `run` or only the state with `exec`.

Instead of tagging state effects it is also possible to transform a State effect acting on a "small" state into a State
effect acting on a "bigger" state:${snippet {
      import org.atnos.eff._, all._, syntax.all.given
      import cats.data.State

      type Count[A] = State[Int, A]
      type Sum[A] = State[Int, A]
      type Mean[A] = State[(Int, Int), A]

      type S1 = Fx.fx1[Count]
      type S2 = Fx.fx1[Sum]
      type S = Fx.fx1[Mean]

      def count(list: List[Int]): Eff[S1, String] = for {
        _ <- put(list.size)
      } yield s"there are ${list.size} values"

      def sum(list: List[Int]): Eff[S2, String] = {
        val s = if (list.isEmpty) 0 else list.sum
        for {
          _ <- put(s)
        } yield s"the sum is $s"
      }

      def mean(list: List[Int]): Eff[S, String] = for {
        m1 <- count(list).lensState((_: (Int, Int))._1, (s: (Int, Int), i: Int) => (i, s._2))
        m2 <- sum(list).lensState((_: (Int, Int))._2, (s: (Int, Int), i: Int) => (s._1, i))
      } yield m1 + "\n" + m2

      mean(List(1, 2, 3)).runState((0, 0)).run
    }.eval}

"""
}
