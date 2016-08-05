package org.atnos.eff

import org.specs2.Specification
import cats.data._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class ReaderEffectSpec extends Specification { def is = s2"""

 local can be used to "zoom" on a configuration $localEffect

 localReader can be used to transform a "small" reader effect into a "bigger" one $localReaderEffect

"""

  def localEffect = {
    type R[A] = Reader[Config, A]
    type S = Fx.fx1[R]

    val action: Eff[S, (Int, String)] = for {
      f <- local[S, Config, Int]((_:Config).factor)
      h <- local[S, Config, String]((_:Config).host)
    } yield (f, h)

    action.runReader(Config(10, "www.me.com")).run ==== ((10, "www.me.com"))
  }

  def localReaderEffect= {
    type ReaderConfig[A] = Reader[Config, A]
    type ReaderInt[A]    = Reader[Int, A]
    type ReaderString[A] = Reader[String, A]

    def readFactor[R :_option](implicit r: ReaderInt |= R): Eff[R, String] = for {
      c <- ask[R, Int]
      h <- OptionEffect.some("hello")
    } yield h

    def readHost[R :_option](implicit r: ReaderString |= R): Eff[R, String] = for {
      c <- ask[R, String]
      h <- OptionEffect.some("world")
    } yield h

    type S1 = Fx.fx3[ReaderInt, ReaderConfig, Option]
    type S2 = Fx.fx3[ReaderString, ReaderConfig, Option]

    def action = for {
      s1 <- readFactor[S1].localReader((c: Config) => c.factor)
      s2 <- readHost[S2].localReader((c: Config) => c.host)
    } yield s1 + " " + s2

    action.runReader(Config(10, "www.me.com")).runOption.run ==== Some("hello world")
  }

  case class Config(factor: Int, host: String)

}

