package org.specs2.control.eff

import com.ambiata.disorder.PositiveIntSmall
import org.scalacheck.Arbitrary._
import org.scalacheck._
import Eff._
import Effects._
import ReaderEffect._
import WriterEffect._
import org.specs2.{ScalaCheck, Specification}
import cats.data._
import cats.syntax.all._
import cats.std.all._


class EffSpec extends Specification with ScalaCheck { def is = s2"""

 The Eff monad respects the laws            $laws

 run the reader monad with a pure operation $readerMonadPure
 run the reader monad with a bind operation $readerMonadBind
 run the writer monad twice                 $writerTwice

 run a reader/writer action $readerWriter

 The Eff monad is stack safe with Writer                 $stacksafeWriter
 The Eff monad is stack safe with Reader                 $stacksafeReader
 The Eff monad is stack safe with both Reader and Writer $stacksafeReaderWriter

"""

  def laws = pending //cats.laws.discipline.MonadTests[F].monad[Int, Int, Int]

  def readerMonadPure = prop { (initial: Int) =>
    type R[A] = Reader[Int, A]
    type S = R |: NoEffect

    run(runReader(initial)(ask[S, Int](ReaderMemberFirst))) === initial
  }

  def readerMonadBind = prop { (initial: Int) =>
    type R[A] = Reader[Int, A]
    type S = R |: NoEffect

    import ReaderImplicits._

    val read: Eff[S, Int] =
      for {
        i <- ask[S, Int]
        j <- ask[S, Int]
      } yield i + j

    run(runReader(initial)(read)) === initial * 2
  }

  def writerTwice = prop { _ : Int =>
    type W[A] = Writer[String, A]
    type S = W |: NoEffect

    val write: Eff[S, Unit] =
      for {
        _ <- tell("hello")
        _ <- tell("world")
      } yield ()

    run(runWriter(write)) ==== (((), List("hello", "world")))
  }

  def readerWriter = prop { init: PositiveIntSmall =>

    // define a Reader / Writer stack
    type W[A] = Writer[String, A]
    type R[A] = Reader[Int, A]
    type S = W |: R |: NoEffect

    object SImplicits extends MemberImplicits with ReaderImplicits with WriterImplicits
    import SImplicits._

    // create actions
    val readWrite: Eff[S, Int] =
      for {
        i <- ask[S, Int]
        _ <- tell[S, String]("initial="+i)
        j <- ask[S, Int]
        _ <- tell[S, String]("result="+(i+j))
      } yield i + j

    // run effects
    val initial = init.value
    run(runReader(initial)(runWriter(readWrite))) must_==
      ((initial * 2, List("initial="+initial, "result="+(initial*2))))
  }

  def stacksafeWriter = {
    type WriterString[A] = Writer[String, A]
    type E = WriterString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => WriterEffect.tell[E, String](i.toString))

    run(WriterEffect.runWriter(action)) ==== ((list.as(()), list.map(_.toString)))
  }

  def stacksafeReader = {
    type ReaderString[A] = Reader[String, A]
    type E = ReaderString |: NoEffect
    import ReaderImplicits._

    val list = (1 to 5000).toList
    val action = list.traverseU(i => ReaderEffect.ask[E, String])

    run(ReaderEffect.runReader("h")(action)) ==== list.as("h")
  }

  def stacksafeReaderWriter = {
    type ReaderString[A] = Reader[String, A]
    type WriterString[A] = Writer[String, A]

    type E = ReaderString |: WriterString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => ReaderEffect.ask[E, String] >>= WriterEffect.tell[E, String])

    run(WriterEffect.runWriter(ReaderEffect.runReader("h")(action))) ==== ((list.as(()), list.as("h")))
  }

  /**
   * Helpers
   */
   type F[A] = Eff[NoEffect, A]

   implicit def ArbitraryEff: Arbitrary[F[Int]] = Arbitrary[F[Int]] {
     Gen.oneOf(
       Gen.choose(0, 100).map(i => EffMonad[NoEffect].pure(i)),
       Gen.choose(0, 100).map(i => EffMonad[NoEffect].pure(i).map(_ + 10))
     )
   }

   implicit def ArbitraryEffFunction: Arbitrary[F[Int => Int]] =
     Arbitrary(arbitrary[Int => Int].map(f => EffMonad[NoEffect].pure(f)))

}
