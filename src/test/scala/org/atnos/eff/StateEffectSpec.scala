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
    import org.atnos.eff.implicits._

    val action: Eff[E, String] = for {
      a <- get[E, Int]
      h <- EffMonad[E].pure("hello")
      _ <- put(a + 5)
      b <- get[E, Int]
      _ <- put(b + 10)
      w <- EffMonad[E].pure("world")
    } yield h+" "+w

    action.runState(5).run ==== (("hello world", 20))
  }

  def modifyState = {
    import org.atnos.eff.implicits._

    val action: Eff[E, String] = for {
       a <- get[E, Int]
       _ <- put(a + 1)
       _ <- modify((_:Int) + 10)
    } yield a.toString

    action.execStateZero.run ==== 11
  }

  def stacksafeState = {
    import org.atnos.eff.implicits._

    val list = (1 to 5000).toList
    val action = list.traverseU(i => StateEffect.put[E, Int](i).as(i.toString))

    action.runState(0).run ==== ((list.map(_.toString), 5000))
  }

  def stateLens = {
    type StateIntPair[A] = State[(Int, Int), A]
    type SS = StateIntPair |: Option |: NoEffect
    type TS = StateInt |: Option |: NoEffect

    implicit val ss1: Member.Aux[StateIntPair, SS, Option |: NoEffect] =
      Member.first

    implicit val ss2: Member.Aux[Option, SS, StateIntPair |: NoEffect] =
      Member.successor

    implicit val ts1: Member.Aux[StateInt, TS, Option |: NoEffect] =
      Member.first

    implicit val ts2: Member.Aux[Option, TS, StateInt |: NoEffect] =
      Member.successor

    val action: Eff[TS, String] =
      for {
        _ <- put[TS, Int](10)
        h <- OptionEffect.some[TS, String]("hello")
        _ <- modify[TS, Int](_ + 2)
      } yield h

    val getter = (pair: (Int, Int)) => pair._2
    val setter = (pair: (Int, Int), j: Int) => (pair._1, j)

    val lensed = lensState(action, getter, setter)

    import org.atnos.eff.implicits._

    lensed.runOption.runState((20, 30)).run ==== ((Some("hello"), (20, 12)))

  }


  type StateInt[A] = State[Int, A]

  type E = StateInt |: NoEffect

}
