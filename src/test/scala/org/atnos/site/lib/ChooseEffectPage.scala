package org.atnos.site
package lib

import cats.Monad

object ChooseEffectPage extends UserGuidePage {
  def is = "Choose".title ^ s2"""

The `Choose` effect is used for non-deterministic computations. With the `Choose` effect you can model computations which either:

  - return no result at all
  - choose between 2 different computations

`Choose` is actually a generalization of `List` where instead of "exploring" all the branches we might "cut" some of them.
That behaviour is controlled by the `Alternative[F]` instance you use when running `Choose`.

For example if we take `List` to run a similar example as before, we get the list of all the accepted pairs: ${snippet {
      import org.atnos.eff._, all._, syntax.all.given

      type S = Fx.fx1[Choose]

// create all the possible pairs for a given list
// where the sum is greater than a value
      def pairsBiggerThan[R: _choose](list: List[Int], n: Int): Eff[R, (Int, Int)] = for {
        a <- chooseFrom(list)
        b <- chooseFrom(list)
        found <-
          if (a + b > n) Monad[Eff[R, *]].pure((a, b))
          else zero
      } yield found

      pairsBiggerThan[S](List(1, 2, 3, 4), 5).runChoose[List].run
    }.eval}

"""
}
