package org.atnos.eff

import org.scalacheck.Arbitrary._
import org.scalacheck._
import org.specs2.{ScalaCheck, Specification}
import cats._
import data._
import cats.syntax.all._
import cats.instances.all._
import cats.Eq
import cats.~>

import scala.collection.mutable.ListBuffer
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

 An effect of the stack can be transformed into another one                        $transformEffect
 An effect of the stack can be translated into other effects on that stack         $translateEffect
 An effect of the stack can be locally translated into other effects on that stack $translateEffectLocal
 An effect can be intercepted and transformed to other values for the same effect  $interceptEffectNat
 An effect can be translated into other effects of the same stack                  $translateIntoEffect

 Applicative calls can be optimised by "batching" requests $optimiseRequests

"""

  def laws =
    pending("wait for discipline to upgrade ScalaCheck to 0.13") // MonadTests[F].monad[Int, Int, Int].all

  type ReaderInt[A]    = Reader[Int, A]
  type ReaderString[A] = Reader[String, A]
  type WriterString[A] = Writer[String, A]
  type StateString[A]  = State[String, A]

  type ReaderIntFx    = Fx.fx1[ReaderInt]
  type ReaderStringFx = Fx.fx1[ReaderString]
  type WriterStringFx = Fx.fx1[WriterString]

  def readerMonadPure = prop { (initial: Int) =>
    ask[ReaderIntFx, Int].runReader(initial).run === initial
  }

  def readerMonadBind = prop { (initial: Int) =>
    val read: Eff[ReaderIntFx, Int] =
      for {
        i <- ask[ReaderIntFx, Int]
        j <- ask[ReaderIntFx, Int]
      } yield i + j

    read.runReader(initial).run === initial * 2
  }

  def writerTwice = prop { _ : Int =>
    val write: Eff[WriterStringFx, Unit] =
      for {
        _ <- tell[WriterStringFx, String]("hello")
        _ <- tell[WriterStringFx, String]("world")
      } yield ()

    write.runWriter.run ==== (((), List("hello", "world")))
  }

  def readerWriter = prop { init: Int =>

    // define a Reader / Writer stack
    type S = Fx.fx2[WriterString, ReaderInt]

    // create actions
    val readWrite: Eff[S, Int] =
      for {
        i <- ask[S, Int]
        _ <- tell[S, String]("init="+i)
        j <- ask[S, Int]
        _ <- tell[S, String]("result="+(i+j))
      } yield i + j

    // run effects
    readWrite.runWriter.runReader(init).run must_== ((init * 2, List("init="+init, "result="+(init*2))))
  }.setGen(Gen.posNum[Int])

  def stacksafeWriter = {
    val list = (1 to 5000).toList
    val action = list.traverseU(i => WriterEffect.tell[WriterStringFx, String](i.toString))

    action.runWriter.run ==== ((list.as(()), list.map(_.toString)))
  }

  def stacksafeReader = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => ReaderEffect.ask[ReaderStringFx, String])

    action.runReader("h").run ==== list.as("h")
  }

  def stacksafeReaderWriter = {
    type S = Fx.fx2[ReaderString, WriterString]

    val list = (1 to 5000).toList
    val action = list.traverse(i => ReaderEffect.ask[S, String] >>= WriterEffect.tell[S, String])

    action.runReader("h").runWriter.run ==== ((list.as(()), list.as("h")))
  }

  def runPureValue =
    (EffMonad[Fx.fx1[Eval]].pure(1).runPure === Option(1)) and
    (delay(1).runPure === None)

  def runOneEffect =
    Eval.later(1).send.runEval.run === 1

  def detachOneEffect =
    delay(1).detach.value === 1

  def traverseEff = {
    val traversed: Eff[Fx.fx1[Option], List[Int]] =
      List(1, 2, 3).traverseA(i => OptionEffect.some(i))
    val flatTraversed: Eff[Fx.fx1[Option], List[Int]] =
      List(1, 2, 3).flatTraverseA(i => OptionEffect.some(List(i, i + 1)))

    traversed.runOption.run === Option(List(1, 2, 3)) &&
      flatTraversed.runOption.run === Option(List(1, 2, 2, 3, 3, 4))
  }

  def functionReader[R, U, A, B](f: A => Eff[R, B])(implicit into: IntoPoly[R, U],
                                                    m: MemberIn[Reader[A, ?], U]): Eff[U, B] =
    ask[U, A].flatMap(f(_).into[U])

  def notInStack = {

    val a: Eff[Fx.fx1[Option], Int] = OptionEffect.some(1)

    val b: Eff[Fx.fx2[ReaderString, Option], Int] = functionReader((s: String) => a.map(_ + s.size))

    b.runReader("start").runOption.run ==== Option(6)
  }

  def inStack = {

    val a: Eff[Fx.fx2[ReaderString, Option], Int] = OptionEffect.some(1)

    val b: Eff[Fx.fx3[ReaderString, ReaderString, Option], Int] = functionReader((s: String) => a.map(_ + s.size))

    b.runReader("start").runReader("start2").runOption.run ==== Option(6)

  }

  def transformEffect = {
    def readSize[R](implicit m: ReaderString |= R): Eff[R, Int] =
      ReaderEffect.ask.map(_.size)

    def setString[R](implicit m: StateString |= R): Eff[R, Unit] =
      StateEffect.put("hello")

    val readerToState = new ~>[ReaderString, StateString] {
      def apply[A](fa: Reader[String, A]): State[String, A] =
        State((s: String) => (s, fa.run(s)))
    }

    type S0 = Fx.fx2[ReaderString, Option]
    type S1 = Fx.fx2[StateString, Option]

    implicit val m1 = Member.Member2L[ReaderString, Option]
    implicit val m2 = Member.Member2L[StateString, Option]

    def both: Eff[S1, Int] = for {
      _ <- setString[S1]
      s <- readSize[S0].transform(readerToState)(m1, m2)
    } yield s

    both.runState("universe").runOption.run ==== Option((5, "hello"))
  }

  def translateEffect = {
    type S0 = Fx.fx3[ReaderString, StateString, Option]
    type S1 = Fx.fx2[StateString, Option]

    def readSize[R](implicit m: ReaderString |= R): Eff[R, Int] =
      ReaderEffect.ask.map(_.size)

    def readerToStateTranslation[R](implicit m: StateString |= R) = new Translate[ReaderString, R] {
      def apply[A](fa: Reader[String, A]): Eff[R, A] =
        Eff.send(State((s: String) => (s, fa.run(s))))
    }

    readSize[S0].translate(readerToStateTranslation[S1]).runState("hello").runOption.run ==== Option((5, "hello"))

  }

  def translateEffectLocal = {
    type S2 = Fx.fx2[StateString, Option]

    def readSize[R](implicit m: ReaderString |= R): Eff[R, Int] =
      ReaderEffect.ask.map(_.size)

    def setString[R](implicit m: StateString |= R): Eff[R, Unit] =
      StateEffect.put("hello")

    def readerToState[R](implicit s: StateString |= R): Translate[ReaderString, R] = new Translate[ReaderString, R] {
      def apply[A](fa: Reader[String, A]): Eff[R, A] =
        send(State((s: String) => (s, fa.run(s))))
    }

    def both[R](implicit s: StateString |= R): Eff[R, Int] = {
      type R1 = Fx.prepend[ReaderString, R]

      val action: Eff[R1, Int] = for {
        _ <- setString[R1]
        s <- readSize[R1]
      } yield s

      action.translate(readerToState)
    }

    both[S2].runState("universe").runOption.run ==== Option((5, "hello"))
  }

  def interceptEffectNat = prop { (n: Int, s: String) =>
    type WS[X] = Writer[String, X]
    type RI[X] = Reader[Int, X]
    type S = Fx2[WS, RI]

    val logs: Eff[S, Unit] =
      (1 to n).toList.traverse(_ => ask[S, Int] >>= (i => tell[S, String](s+i.toString))).void

    val logsA: Eff[S, Unit] =
      (1 to n).toList.traverseA(_ => ask[S, Int] >>= (i => tell[S, String](s+i.toString))).void

    def reverse(ls: Eff[S, Unit]) =
      interpret.interceptNat(ls)(new (WS ~> WS) {
        def apply[X](w: WS[X]): WS[X] =
          w.run match { case (l, v) => Writer.apply(l.reverse, v) }
      })

    val reversed  = reverse(logs).runWriterLog.runReader(0).run
    val reversedA = reverse(logsA) .runWriterLog.runReader(0).run
    val expected = (1 to n).map(_ => (s+"0").reverse).toList

    (reversed ==== expected) and (reversedA ==== expected)

  }.setGens(Gen.choose(3, 3), Gen.oneOf("abc", "dce", "xyz")).set(minTestsOk = 1)

  def translateIntoEffect = prop { n: Int =>
    sealed trait Stored[A]
    case class Get(k: String)            extends Stored[Unit]
    case class Update(k: String, i: Int) extends Stored[Unit]
    case class Remove(k: String)         extends Stored[Unit]

    type WriterString[A] = Writer[String, A]

    def runStored[R, U, A](e: Eff[R, A])(implicit m: Member.Aux[Stored, R, U]): Eff[U, A] =
      interpret.translate[R, U, Stored, A](e)(new Translate[Stored, U] {
        def apply[X](tx: Stored[X]) = pure[U, X](().asInstanceOf[X])
      })


    val w =
      new Write[Stored, String] {
        def apply[X](tx: Stored[X]) = tx match {
          case Get(k)       => k
          case Update(k, _) => k
          case Remove(k)    => k
        }
      }

    val wAugment =
      new Augment[Stored, Writer[String, ?]] {
        def apply[X](tx: Stored[X]) = tx match {
          case Get(k) => Writer.tell(k)
          case Update(k, _) => Writer.tell(k)
          case Remove(k) => Writer.tell(k)
        }
      }

    type R1 = Fx.fx1[Stored]
    type R2 = Fx.fx2[WriterString, Stored]

    val action: Eff[R1, Unit] =
      send[Stored, R1, Unit](Update("a", 1)) >>
      send[Stored, R1, Unit](Get("b"))       >>
      send[Stored, R1, Unit](Remove("c"))

    (runStored(action.write(w)).runWriterLog.run ==== List("a", "b", "c")) &&
      (runStored(action.augment(wAugment)).runWriterLog.run ==== List("a", "b", "c"))
  }

  def optimiseRequests = {

    // An effect to get users from a database
    // calls can be individual or batched
    case class User(i: Int)
    sealed trait UserDsl[+A]

    case class GetUser(i: Int) extends UserDsl[User]
    case class GetUsers(is: List[Int]) extends UserDsl[List[User]]
    type _userDsl[R] = UserDsl |= R

    implicit def BatchableUserDsl: Batchable[UserDsl] = new Batchable[UserDsl] {
      type Z = List[User]
      type E = User
      def distribute(z: Z): List[E] = z

      def batch[X, Y](tx: UserDsl[X], ty: UserDsl[Y]): Option[UserDsl[Z]] = Option {
        (tx, ty) match {
          case (GetUser(i),   GetUser(j))   => GetUsers(List(i, j))
          case (GetUser(i),   GetUsers(is)) => GetUsers(i :: is)
          case (GetUsers(is), GetUser(i))   => GetUsers(is :+ i)
          case (GetUsers(is), GetUsers(js)) => GetUsers(is ++ js)
        }
      }
    }

    def getUser[R :_userDsl](i: Int): Eff[R, User] =
      send[UserDsl, R, User](GetUser(i))

    def getUsers[R :_userDsl](is: List[Int]): Eff[R, List[User]] =
      send[UserDsl, R, List[User]](GetUsers(is))

    def getWebUser(i: Int): User = User(i)
    def getWebUsers(is: List[Int]): List[User] = is.map(i => User(i))

    def runDsl[A](eff: Eff[Fx1[UserDsl], A]): A =
      eff match {
        case Pure(a, _) => a
        case Impure(UnionTagged(GetUser(i), _), c, _)   => runDsl(c(getWebUser(i)))
        case Impure(UnionTagged(GetUsers(is), _), c, _) => runDsl(c(getWebUsers(is)))
        case ap @ ImpureAp(u, m, _)                     => runDsl(ap.toMonadic)
        case Impure(_, _, _)                            => sys.error("this should not happen with just one effect")
      }

    def action1[R :_userDsl] =
      Eff.traverseA(List(1, 2))(i => getUser(i))

    val action = action1

    val optimised = action1.batch

    val result = runDsl(action)
    val optimisedResult = runDsl(optimised)

    result ==== optimisedResult
  }


  /**
   * Helpers
   */
  type F[A] = Eff[Fx.fx1[Option], A]

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
