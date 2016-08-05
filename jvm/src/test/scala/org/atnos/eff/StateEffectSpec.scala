package org.atnos.eff

import org.specs2.{ScalaCheck, Specification}
import cats.syntax.all._
import cats.std.int._
import cats.std.list.listInstance
import cats.data._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class StateEffectSpec extends Specification with ScalaCheck { def is = s2"""

 The state monad can be used to put/get state $putGetState
 modify can be used to modify the current state $modifyState

 A State[T, ?] effect can be lifted into a State[S, ?] effect provided there is a
 "lens" (a getter and a setter) from T to S $stateLens

 The Eff monad is stack safe with State $stacksafeState

"""

  def putGetState = {

    def action[R:_stateInt]: Eff[R, String] = for {
      a <- get
      h <- EffMonad.pure("hello")
      _ <- put(a + 5)
      b <- get
      _ <- put(b + 10)
      w <- EffMonad.pure("world")
    } yield h+" "+w

    action[E].runState(5).run ==== (("hello world", 20))
  }

  def modifyState = {

    def action[R :_stateInt]: Eff[R, String] = for {
       a <- get
       _ <- put(a + 1)
       _ <- modify((_:Int) + 10)
    } yield a.toString

    action[E].execStateZero.run ==== 11
  }

  def stacksafeState = {

    val list = (1 to 5000).toList
    val action = list.traverseU(i => StateEffect.put[E, Int](i).as(i.toString))

    action.runState(0).run ==== ((list.map(_.toString), 5000))
  }

  def stateLens = {
    type StateIntPair[A] = State[(Int, Int), A]
    type SS = Fx.fx2[StateIntPair, Option]
    type TS = Fx.fx2[StateInt, Option]

    val action: Eff[TS, String] =
      for {
        _ <- put[TS, Int](10)
        h <- OptionEffect.some[TS, String]("hello")
        _ <- modify[TS, Int](_ + 2)
      } yield h

    val getter = (pair: (Int, Int)) => pair._2
    val setter = (pair: (Int, Int), j: Int) => (pair._1, j)

    val lensed = lensState(action, getter, setter)


    lensed.runOption.runState((20, 30)).run ==== ((Some("hello"), (20, 12)))

  }


  type StateInt[A] = State[Int, A]

  type E = Fx.fx1[StateInt]
  type _stateInt[R] = StateInt |= R

}
