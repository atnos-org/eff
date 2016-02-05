package org.specs2.control.eff

import Eff._
import Effects._
import org.specs2.{ScalaCheck, Specification}
import StateEffect._
import cats.syntax.all._
import cats.std.int._
import cats.std.list.listInstance
import cats.data._

class StateEffectSpec extends Specification with ScalaCheck { def is = s2"""

 The state monad can be used to put/get state $putGetState
 modify can be used to modify the current state $modifyState

 The Eff monad is stack safe with State $stacksafeState

"""

  def putGetState = {
    val action: Eff[E, String] = for {
      a <- get[E, Int]
      h <- EffMonad[E].pure("hello")
      _ <- put(a + 5)
      b <- get[E, Int]
      _ <- put(b + 10)
      w <- EffMonad[E].pure("world")
    } yield h+" "+w

    run(runState(5)(action)) ==== (("hello world", 20))
  }

  def modifyState = {
    val action: Eff[E, String] = for {
       a <- get[E, Int]
       _ <- put(a + 1)
       _ <- modify((_:Int) + 10)
    } yield a.toString

    run(execZero(action)) ==== 11
  }

  def stacksafeState = {
    val list = (1 to 5000).toList
    val action = list.traverseU(i => StateEffect.put[E, Int](i).as(i.toString))

    run(StateEffect.runState(0)(action)) ==== ((list.map(_.toString), 5000))
  }

  type StateInt[A] = State[Int, A]

  type E = StateInt |: NoEffect

}
