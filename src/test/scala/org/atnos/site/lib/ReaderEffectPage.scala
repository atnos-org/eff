package org.atnos.site
package lib

object ReaderEffectPage extends UserGuidePage { def is = "Reader".title ^ s2"""

The `Reader` effect is used to request values from an "environment". The main method is `ask` to get the current environment
(or "configuration" if you prefer to see it that way) and you can run an effect stack containing a `Reader` effect by
providing a value for the environment with the `runReader` method.

You can also inject a "local" reader into a "bigger" one:${snippet {
import org.atnos.eff._, all._, syntax.all._
import cats.data._

case class Conf(host: String, port: Int)

type R1[A] = Reader[Int, A]
type R2[A] = Reader[Conf, A]

type S = Fx.fx2[R1, R2]

def getPort[R](implicit r: Reader[Int, *] |= R): Eff[R, String] = for {
  p1 <- ask[R, Int]
} yield "the port is " + p1

getPort[S].translateReader((_: Conf).port).runReader(Conf("prod", 80)).run
}.eval}

"""
}
