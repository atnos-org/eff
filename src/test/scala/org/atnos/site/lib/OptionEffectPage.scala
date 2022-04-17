package org.atnos.site
package lib

object OptionEffectPage extends UserGuidePage {
  def is = "Option".title ^ s2"""

Adding an `Option` effect in your stack allows to stop computations when necessary.
If you create a value with `some(a)` this value will be used downstream but if you use `none` all computations will stop:${snippet {
      import org.atnos.eff._, all._, syntax.all._

      /**
 * Stack declaration
 */
      type S = Fx.fx1[Option]

// compute with this stack
      val map: Map[String, Int] =
        Map("key1" -> 10, "key2" -> 20)

// get 2 keys from the map and add the corresponding values
      def addKeys(key1: String, key2: String): Eff[S, Int] = for {
        a <- fromOption(map.get(key1))
        b <- fromOption(map.get(key2))
      } yield a + b

      (addKeys("key1", "key2").runOption.run, addKeys("key1", "missing").runOption.run)
    }.eval}


"""
}
