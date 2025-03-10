package org.atnos.site
package lib

object EitherEffectPage extends UserGuidePage {
  def is = "Either".title ^ s2"""

The `Either` effect is similar to the `Option` effect but adds the possibility to specify why a computation stopped: ${snippet {
      import org.atnos.eff._, all._, syntax.all.given

      /**
   * Stack declaration
   */
      type S = Fx.fx1[Either[String, *]]

      // compute with this stack
      val map: Map[String, Int] =
        Map("key1" -> 10, "key2" -> 20)

      // get 2 keys from the map and add the corresponding values
      def addKeys(key1: String, key2: String): Eff[S, Int] = for {
        a <- optionEither(map.get(key1), s"'$key1' not found")
        b <- optionEither(map.get(key2), s"'$key2' not found")
      } yield a + b

      (addKeys("key1", "key2").runEither.run, addKeys("key1", "missing").runEither.run)
    }.eval}

*Note*: the `*` syntax comes from the [kind-projector](https://github.com/typelevel/kind-projector) project and allows us to avoid
type lambdas.

A `catchLeft` method can also be used to intercept an error and possibly recover from it:${snippet {
      // 8<--
      import org.atnos.eff._, all._, syntax.all.given
      // 8<--
      case class TooBig(value: Int)
      type E = Fx.fx1[Either[TooBig, *]]

      val i = 7

      val value: Eff[E, Int] =
        if (i > 5) left[E, TooBig, Int](TooBig(i))
        else right[E, TooBig, Int](i)

      val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) { case TooBig(k) =>
        if (k < 10) right[E, TooBig, Int](k)
        else left[E, TooBig, Int](TooBig(k))
      }

      action.runEither.run ==== Right(7)
    }}

*Note*: the type annotations on `left` and `right` can be avoided by adding an implicit declaration in scope. You can learn
more about this in the ${"Implicits" ~/ MemberImplicits} section.


"""
}
