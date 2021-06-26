package org.atnos.eff

import org.scalacheck.Arbitrary._
import org.scalacheck._
import org.specs2.{ScalaCheck, Specification}
import cats.syntax.all.{ catsSyntaxEq => _, _ }
import cats.Eq
import org.atnos.eff.EffCompat._
import org.atnos.eff.Batchable
import org.atnos.eff.all._
import org.atnos.eff.concurrent.Scheduler
import org.atnos.eff.future._
import org.atnos.eff.syntax.all._
import org.atnos.eff.syntax.future._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.ThrownExpectations

import scala.concurrent._
import duration._
import scala.collection.mutable.ListBuffer

class EffApplicativeSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 *> uses the applicative "sequencing" whereas >> uses the monadic sequencing $operators
 This means that *> will discard the left result but can still run 2 actions concurrently

 Eff values can be traversed with an applicative instance $traverseEff
 Eff.traverseA is stacksafe                               $traverseAStacksafe
 Eff.traverseA preserves concurrency                      $traverseAConcurrent

 Applicative calls can be optimised by "batching" requests $optimiseRequests
 Interleaved applicative calls can be interpreted properly
   with no natural interpretation $interleavedApplicative1 (see release notes for 4.0.2)
   with a natural interpretation  $interleavedApplicative2 (see release notes for 4.4.2)

"""

  def operators = {
    type S = Fx.fx2[Option, TimedFuture]
    val messages: ListBuffer[String] = new ListBuffer[String]

    val action1: Eff[S, Unit] =
      OptionEffect.some[S, Int](1) >>
      futureDelay[S, Unit] { Thread.sleep(200); messages.append("action1") }

    val action2: Eff[S, Int] =
      OptionEffect.some[S, Int](2) >>
      futureDelay[S, Int] { messages.append("action2"); 2 }

    (action1 >> action2).runOption.runAsync must beSome.await
    (action1 *> action2).runOption.runAsync must beSome.await

    messages.toList ==== List("action1", "action2", "action2", "action1")

  }

  def traverseEff = {
    val traversed: Eff[Fx.fx1[Option], List[Int]] =
      List(1, 2, 3).traverseA(i => OptionEffect.some(i))

    val flatTraversed: Eff[Fx.fx1[Option], List[Int]] =
      List(1, 2, 3).flatTraverseA(i => OptionEffect.some(List(i, i + 1)))

    traversed.runOption.run === Option(List(1, 2, 3)) &&
      flatTraversed.runOption.run === Option(List(1, 2, 2, 3, 3, 4))
  }

  def traverseAStacksafe = {
    val list = (1 to 5000).toList

    val traversed: Eff[Fx.fx1[Option], List[Int]] =
      list.traverseA(i => OptionEffect.some(i))

    traversed.runOption.run === Option(list)
  }

  def traverseAConcurrent = {
    val list = (1 to 5000).toList
    type S = Fx.fx2[Option, TimedFuture]
    val messages: ListBuffer[Int] = new ListBuffer[Int]

    val traversed: Eff[S, List[Int]] =
      list.traverseA { i =>
        OptionEffect.some[S, Int](i) >>
          futureDelay[S, Int] { messages.append(i); i }
      }

    traversed.runOption.runAsync must beSome(list).awaitFor(20.seconds)

    messages.toList must not(beEqualTo(messages.toList.sorted))
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

    def getWebUser(i: Int): User = User(i)
    def getWebUsers(is: List[Int]): List[User] = is.map(i => User(i))

    def runDsl[A](eff: Eff[Fx1[UserDsl], A]): A =
      eff match {
        case Pure(a, _) =>
          a
        case Impure(NoEffect(a), c, _) =>
          runDsl(c(a))
        case Impure(UnionTagged(GetUser(i), _), c, _) =>
          runDsl(c.cast[Continuation[Fx1[UserDsl], User, A]].apply(getWebUser(i)))
        case Impure(UnionTagged(GetUsers(is), _), c, _) =>
          runDsl(c.cast[Continuation[Fx1[UserDsl], List[User], A]].apply((getWebUsers(is))))
        case ap @ ImpureAp(u, m, _) =>
          runDsl(ap.toMonadic)
        case Impure(_, _, _) =>
          sys.error("this should not happen with just one effect. Got "+eff)
      }

    def action1[R :_userDsl] =
      Eff.traverseA(List(1, 2))(i => getUser(i))

    val action = action1

    val optimised = action1.batch

    val result = runDsl(action)
    val optimisedResult = runDsl(optimised)

    result ==== optimisedResult
  }

  def interleavedApplicative1 = {
    type S = Fx2[Option, Either[String, *]]
    val action = (1 to 4).toList.traverseA(i =>
      if (i % 2 == 0) OptionEffect.some[S, Int](i) else EitherEffect.right[S, String, Int](i))

    action.runOption.runEither.run ==== Right(Some(List(1, 2, 3, 4)))
  }

  def interleavedApplicative2 = {
    type S = Fx2[Option, TimedFuture]
    val action = (1 to 4).toList.traverseA(i =>
      if (i % 2 == 0) OptionEffect.some[S, Int](i) else FutureEffect.futureDelay[S, Int](i))

    FutureEffect.futureAttempt(action).runOption.runAsync must beSome(Right(List(1, 2, 3, 4)): Either[Throwable, List[Int]]).await
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

  implicit val scheduler: Scheduler =
    ExecutorServices.schedulerFromGlobalExecutionContext

}
