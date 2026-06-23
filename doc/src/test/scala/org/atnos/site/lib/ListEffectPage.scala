package org.atnos.site
package lib

object ListEffectPage extends UserGuidePage {
  def is = "List".title ^ s2"""

The `List` effect is used for computations which may return several values.
A simple example using this effect would be ${snippet {
      import org.atnos.eff._, all._, syntax.all.given

      type S = Fx.fx1[List]

// create all the possible pairs for a given list
// where the sum is greater than a value
      def pairsBiggerThan[R: _list](list: List[Int], n: Int): Eff[R, (Int, Int)] = for {
        a <- values(list*)
        b <- values(list*)
        found <-
          if (a + b > n) singleton((a, b))
          else empty
      } yield found

      pairsBiggerThan[S](List(1, 2, 3, 4), 5).runList.run
    }.eval}

"""
}
