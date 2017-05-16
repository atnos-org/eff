package org.atnos.eff

import cats.Eval
import org.specs2.Specification
import cats.data._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class ReaderEffectSpec extends Specification { def is = s2"""

 local can be used to "zoom" on a configuration $localEffect
   localKleisli for the Kleisli effect $localKleisliEffect

 localReader can be used to transform a "small" reader effect into a "bigger" one $localReaderEffect

 modifyReader can be used to transform a "small" reader effect into a "bigger" one
   and stay in the same stack $modifyReaderEffect

 updateReader can be used to modify the read value (but keep the same type for that value) $updateReaderEffect

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

  def localKleisliEffect = {
    import cats.syntax.option._
    type S = Fx.fx2[Kleisli[Option, Config, ?], Option]

    val action: Eff[S, (Int, String)] = for {
      f <- localKleisli[S, Config, Int, Option]((_:Config).factor.some)
      h <- localKleisli[S, Config, String, Option]((_:Config).host.some)
    } yield (f, h)

    action.runKleisli(Config(10, "www.me.com")).runOption.run ==== Some((10, "www.me.com"))
  }

  def localReaderEffect = {
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
      s1 <- readFactor[S1].translateReader((c: Config) => c.factor)
      s2 <- readHost[S2].translateReader((c: Config) => c.host)
    } yield s1 + " " + s2

    action.runReader(Config(10, "www.me.com")).runOption.run ==== Some("hello world")
  }

  def modifyReaderEffect = {
    type ReaderEnv[A] = Reader[Env, A]
    type Comp = Fx.fx2[ReaderEnv, Option]
    type Env = Map[String, Int]

    val env: Env = Map()

    def lookup(x: String): Eff[Comp, Int] = for {
      e <- ask[Comp, Env]
      v <- OptionEffect.fromOption[Comp, Int](e.get(x))
    } yield v

    // the lookup should work on the modified environment
    // but this should not change subsequent calls to the environment
    def program: Eff[Comp, String] = for {
      v <- lookup("x").zoomReader((_:Env).updated("x", 2))
      e <- ask[Comp, Env]
    } yield s"Value: $v, env: $e"

    program.runReader(env).runOption.run ==== Option(s"Value: 2, env: Map()")
  }


  def updateReaderEffect = {
    type ReaderInt[A] = Reader[Int, A]
    type _ReaderInt[R] = ReaderInt /= R

    type Stack = Fx.fx2[ReaderInt, Eval]

    def bar[R :_ReaderInt :_eval](x: String): Eff[R, String] = for {
      y <- ask[R, Int]
      r <- if (y == 1) delay[R, String](x) else bar(x).localReader((z: Int) => z - 1)
    } yield r + "."

    bar[Stack]("x").runReader(3).runEval.run ==== "x..."

  }

  case class Config(factor: Int, host: String)

}

