package org.atnos.site
package lib

object ValidateEffectPage extends UserGuidePage {
  def is = "Validate".title ^ s2"""

The `Validate` effect is similar to the `Either` effect but let you accumulate failures: ${snippet {
      import org.atnos.eff._, all._, syntax.all.given

      /**
   * Stack declaration
   */
      type S = Fx.fx1[Validate[String, *]]

      def checkPositiveInt(i: Int): Eff[S, Unit] =
        validateCheck(i >= 0, s"$i is not positive")

      def checkPositiveInts(a: Int, b: Int, c: Int): Eff[S, (Int, Int, Int)] = for {
        _ <- checkPositiveInt(a)
        _ <- checkPositiveInt(b)
        _ <- checkPositiveInt(c)
      } yield (a, b, c)

      checkPositiveInts(1, -3, -2).runNel.run
    }.eval}

"""
}
