package org.atnos.eff

import org.scalacheck.Arbitrary._
import org.scalacheck._
import org.specs2.{ScalaCheck, Specification}
import cats._
import data._
import cats.syntax.all._
import cats.std.all._
import cats.Eq
import cats.arrow.NaturalTransformation
import org.atnos.eff.Interpret.Translate
//import cats.laws.discipline.{arbitrary => _, _}
//import CartesianTests._, Isomorphisms._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class EffSpec extends Specification with ScalaCheck { def is = s2"""

 The Eff monad respects the laws            $laws

 run the reader monad with a pure operation $readerMonadPure
 run the reader monad with a bind operation $readerMonadBind
 run the writer monad twice                 $writerTwice

 run a reader/writer action $readerWriter

 The Eff monad is stack safe with Writer                 $stacksafeWriter
 The Eff monad is stack safe with Reader                 $stacksafeReader
 The Eff monad is stack safe with both Reader and Writer $stacksafeReaderWriter

 It is possible to run a pure Eff value $runPureValue
 It is possible to run a Eff value with one effects $runOneEffect
 It is possible to run a Eff value with just one effect and detach it back $detachOneEffect

 Eff values can be traversed with an applicative instance $traverseEff

 A stack can be added a new effect when the effect is not in stack $notInStack
 A stack can be added a new effect when the effect is in stack     $inStack

 An effect of the stack can be transformed into another one        $transformEffect
 An effect of the stack can be translated into other effects on that stack $translateEffect
 An effect of the stack can be locally translated into other effects on that stack $translateEffectLocal

"""

  def laws =
    pending("wait for discipline to upgrade ScalaCheck to 0.13") // MonadTests[F].monad[Int, Int, Int].all

  def readerMonadPure = prop { (initial: Int) =>
    type R[A] = Reader[Int, A]
    type S = R |: NoEffect

    ask[S, Int].runReader(initial).run === initial
  }

  def readerMonadBind = prop { (initial: Int) =>
    type R[A] = Reader[Int, A]
    type S = R |: NoEffect

    val read: Eff[S, Int] =
      for {
        i <- ask[S, Int]
        j <- ask[S, Int]
      } yield i + j

    read.runReader(initial).run === initial * 2
  }

  def writerTwice = prop { _ : Int =>
    type W[A] = Writer[String, A]
    type S = W |: NoEffect

    val write: Eff[S, Unit] =
      for {
        _ <- tell("hello")
        _ <- tell("world")
      } yield ()

    write.runWriter.run ==== (((), List("hello", "world")))
  }

  def readerWriter = prop { init: Int =>

    // define a Reader / Writer stack
    type W[A] = Writer[String, A]
    type R[A] = Reader[Int, A]
    type S = W |: R |: NoEffect

    object SImplicits extends MemberImplicits
    import SImplicits._

    // create actions
    val readWrite: Eff[S, Int] =
      for {
        i <- ask[S, Int]
        _ <- tell[S, String]("init="+i)
        j <- ask[S, Int]
        _ <- tell[S, String]("result="+(i+j))
      } yield i + j

    // run effects
    readWrite.runWriter.runReader(init).run must_==
      ((init * 2, List("init="+init, "result="+(init*2))))
  }.setGen(Gen.posNum[Int])

  def stacksafeWriter = {
    type WriterString[A] = Writer[String, A]
    type E = WriterString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverseU(i => WriterEffect.tell[E, String](i.toString))

    action.runWriter.run ==== ((list.as(()), list.map(_.toString)))
  }

  def stacksafeReader = {
    type ReaderString[A] = Reader[String, A]
    type E = ReaderString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverse(i => ReaderEffect.ask[E, String])

    action.runReader("h").run ==== list.as("h")
  }

  def stacksafeReaderWriter = {
    type ReaderString[A] = Reader[String, A]
    type WriterString[A] = Writer[String, A]

    type E = ReaderString |: WriterString |: NoEffect

    val list = (1 to 5000).toList
    val action = list.traverse(i => ReaderEffect.ask[E, String] >>= WriterEffect.tell[E, String])

    action.runReader("h").runWriter.run ==== ((list.as(()), list.as("h")))
  }

  def runPureValue =
    (EffMonad[Eval |: NoEffect].pure(1).runPure === Option(1)) and
    (delay(1).runPure === None)

  def runOneEffect =
    delay(1).runEval.run === 1

  def detachOneEffect =
    delay(1).detach.value === 1

  def traverseEff = {
    type R = Option |: NoEffect
    val traversed: Eff[R, List[Int]] =
      List(1, 2, 3).traverseA(i => OptionEffect.some(i))

    traversed.runOption.run === Option(List(1, 2, 3))
  }


  def functionReader[R, A, B](f: A => Eff[R, B]): Eff[Reader[A, ?] |: R, B] =
    ask[Reader[A, ?] |: R, A].flatMap(f(_).into[Reader[A, ?] |: R])

  def notInStack = {

    type S = Option |: NoEffect

    val a: Eff[S, Int] = OptionEffect.some(1)

    val b: Eff[Reader[String, ?] |: S, Int] = functionReader((s: String) => a.map(_ + s.size))

    b.runReader("start").runOption.run ==== Option(6)
  }

  def inStack = {

    type S = Reader[String, ?] |: Option |: NoEffect

    val a: Eff[S, Int] = OptionEffect.some(1)

    val b: Eff[Reader[String, ?] |: S, Int] = functionReader((s: String) => a.map(_ + s.size))

    b.runReader("start").runReader("start2").runOption.run ==== Option(6)

  }

  def transformEffect = {
    type S = Reader[String, ?] |: Option |: NoEffect
    type S2 = State[String, ?] |: Option |: NoEffect

    def readSize[R](implicit m: Member[Reader[String, ?], R]): Eff[R, Int] =
      ReaderEffect.ask.map(_.size)

    def setString[R](implicit m: Member[State[String, ?], R]): Eff[R, Unit] =
      StateEffect.put("hello")

    val readerToState = new NaturalTransformation[Reader[String, ?], State[String, ?]] {
      def apply[A](fa: Reader[String, A]): State[String, A] =
        State((s: String) => (s, fa.run(s)))
    }

    def both: Eff[S2, Int] = for {
      _ <- setString[S2]
      s <- readSize[S].transform(readerToState)
    } yield s

    both.runState("universe").runOption.run ==== Option((5, "hello"))
  }

  def translateEffect = {
    type S = Reader[String, ?] |: State[String, ?] |: Option |: NoEffect
    type S2 = State[String, ?] |: Option |: NoEffect

    def readSize[R](implicit m: Member[Reader[String, ?], R]): Eff[R, Int] =
      ReaderEffect.ask.map(_.size)

    val readerToStateTranslation = new Interpret.Translate[Reader[String, ?], S2] {
      def apply[A](fa: Reader[String, A]): Eff[S2, A] =
        Eff.send(State((s: String) => (s, fa.run(s))))
    }

    implicit val m: Member.Aux[Reader[String, ?], S, S2] = Member.first

    readSize.translate(readerToStateTranslation).runState("hello").runOption.run ==== Option((5, "hello"))

  }

  def translateEffectLocal = {
    type S2 = State[String, ?] |: Option |: NoEffect

    def readSize[R](implicit m: Member[Reader[String, ?], R]): Eff[R, Int] =
      ReaderEffect.ask.map(_.size)

    def setString[R](implicit m: Member[State[String, ?], R]): Eff[R, Unit] =
      StateEffect.put("hello")

    def readerToState[R](implicit s: State[String, ?] <= R): Translate[Reader[String, ?], R] = new Translate[Reader[String, ?], R] {
      def apply[A](fa: Reader[String, A]): Eff[R, A] =
        send(State((s: String) => (s, fa.run(s))))
    }

    def both[R](implicit s: State[String, ?] <= R): Eff[R, Int] = {
      type R1 = Reader[String, ?] |: R

      val action: Eff[R1, Int] = for {
        _ <- setString[R1]
        s <- readSize[R1]
      } yield s

      action.translate(readerToState)
    }

    both[S2].runState("universe").runOption.run ==== Option((5, "hello"))
  }

  /**
   * Helpers
   */
  type F[A] = Eff[Option |: NoEffect, A]

  implicit def ArbitraryEff[R]: Arbitrary[Eff[R, Int]] = Arbitrary[Eff[R, Int]] {
    Gen.oneOf(
      Gen.choose(0, 100).map(i => EffMonad[R].pure(i)),
      Gen.choose(0, 100).map(i => EffMonad[R].pure(i).map(_ + 10))
    )
  }

  implicit def ArbitraryEffFunction[R]: Arbitrary[Eff[R, Int => Int]] =
    Arbitrary(arbitrary[Int => Int].map(f => EffMonad[R].pure(f)))

  import OptionEffect._

  implicit val eqEffInt: Eq[F[Int]] = new Eq[F[Int]] {
    def eqv(x: F[Int], y: F[Int]): Boolean =
      runOption(x).run == runOption(y).run
  }
  implicit val eqEffInt3: Eq[F[(Int, Int, Int)]] = new Eq[F[(Int, Int, Int)]] {
    def eqv(x: F[(Int, Int, Int)], y:F[(Int, Int, Int)]): Boolean =
      runOption(x).run == runOption(y).run
  }

}
