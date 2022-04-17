package org.atnos.site
package lib

object EvalEffectPage extends UserGuidePage {
  def is = "Eval".title ^ s2"""

This effect is a very simple one. It allows the delayed execution of computations and it can serve as some sort of overall `IO` effect.

Two methods are available to execute this effect:

 - `runEval: Eff[U, A]` to just execute the computations

 - `attemptEval: Eff[U, Throwable Either A]` to execute the computations but also catch any `Throwable` that would be thrown

${snippet {
      import org.atnos.eff._, all._, syntax.all._

      delay(1 + 1).runEval.run
    }.eval}

"""
}
