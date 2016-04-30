package org.atnos.eff

import org.specs2.Specification
import cats.data._
import cats.syntax.all._
import org.atnos.eff.all._
import org.atnos.eff.implicits._
import org.atnos.eff.syntax.all._

class ReaderEffectSpec extends Specification { def is = s2"""

 local can be used to "zoom" on a configuration $localEffect

 localReader can be used to transform a "small" reader effect into a "bigger" one $localReaderEffect

"""

  def localEffect = {
    type R[A] = Reader[Config, A]
    type S = R |: NoEffect

    val action: Eff[S, (Int, String)] = for {
      f <- local[S, Config, Int]((_:Config).factor)
      h <- local[S, Config, String]((_:Config).host)
    } yield (f, h)

    action.runReader(Config(10, "www.me.com")).run ==== ((10, "www.me.com"))
  }

  def localReaderEffect= {
    type R[A] = Reader[Config, A]
    type ReaderInt[A] = Reader[Int, A]
    type ReaderString[A] = Reader[String, A]

    type S1 = ReaderInt |: Option |: NoEffect
    type S2 = ReaderString |: Option |: NoEffect
    type SS = R |: Option |: NoEffect

    val readFactor: Eff[S1, String] = for {
      c <- ask[S1, Int]
      h <- OptionEffect.some[S1, String]("hello")
    } yield h

    val readHost: Eff[S2, String] = for {
      c <- ask[S2, String]
      h <- OptionEffect.some[S2, String]("world")
    } yield h

    val action: Eff[SS, String] = for {
      s1 <- localReader(readFactor, (c: Config) => c.factor)
      s2 <- localReader(readHost, (c: Config) => c.host)
    } yield s1 + " " + s2

    action.runReader(Config(10, "www.me.com")).runOption.run ==== Some("hello world")
  }

  case class Config(factor: Int, host: String)

}

