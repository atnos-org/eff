package org.atnos.eff

import cats.Eq
import cats.Monad
import org.atnos.eff.concurrent.Scheduler
import org.atnos.eff.future.*
import org.atnos.eff.syntax.all.given
import org.atnos.eff.syntax.future.given
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.specs2.ScalaCheck
import org.specs2.Specification
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.ThrownExpectations
import scala.collection.mutable.ListBuffer
import scala.concurrent.*
import scala.concurrent.duration.*

class EffApplicativeSpec(using ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations {
  def is = s2"""

 *> uses the applicative "sequencing" whereas >> uses the monadic sequencing $operators
 This means that *> will discard the left result but can still run 2 actions concurrently

 Eff values can be traversed with an applicative instance $traverseEff
 Eff.traverseA is stacksafe                               $traverseAStacksafe
 Eff.traverseA preserves concurrency                      $traverseAConcurrent

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

    Await.result((action1 >> action2).runOption.runAsync, 10.seconds) must beSome
    Await.result((action1 *> action2).runOption.runAsync, 10.seconds) must beSome

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
    val lock = new Object
    val messages: ListBuffer[Int] = new ListBuffer[Int]

    val traversed: Eff[S, List[Int]] =
      list.traverseA { i =>
        OptionEffect.some[S, Int](i) >>
          futureDelay[S, Int] {
            lock.synchronized {
              messages.append(i)
            }
            i
          }
      }

    traversed.runOption.runAsync must beSome(list).awaitFor(20.seconds)
    messages.size === list.size
    messages.toList must not(beEqualTo(messages.toList.sorted))
  }

  def interleavedApplicative1 = {
    type S = Fx2[Option, Either[String, *]]
    val action = (1 to 4).toList.traverseA(i => if (i % 2 == 0) OptionEffect.some[S, Int](i) else EitherEffect.right[S, String, Int](i))

    action.runOption.runEither.run ==== Right(Some(List(1, 2, 3, 4)))
  }

  def interleavedApplicative2 = {
    type S = Fx2[Option, TimedFuture]
    val action = (1 to 4).toList.traverseA(i => if (i % 2 == 0) OptionEffect.some[S, Int](i) else FutureEffect.futureDelay[S, Int](i))

    FutureEffect.futureAttempt(action).runOption.runAsync must beSome(Right(List(1, 2, 3, 4)): Either[Throwable, List[Int]]).await
  }

  /**
   * Helpers
   */
  type F[A] = Eff[Fx.fx1[Option], A]

  given ArbitraryEff[R]: Arbitrary[Eff[R, Int]] = Arbitrary[Eff[R, Int]] {
    Gen.oneOf(
      Gen.choose(0, 100).map(i => Monad[Eff[R, *]].pure(i)),
      Gen.choose(0, 100).map(i => Monad[Eff[R, *]].pure(i).map(_ + 10))
    )
  }

  given ArbitraryEffFunction[R]: Arbitrary[Eff[R, Int => Int]] =
    Arbitrary(arbitrary[Int => Int].map(f => Monad[Eff[R, *]].pure(f)))

  import OptionEffect._

  given eqEffInt: Eq[F[Int]] = (x: F[Int], y: F[Int]) => runOption(x).run == runOption(y).run

  given eqEffInt3: Eq[F[(Int, Int, Int)]] = (x: F[(Int, Int, Int)], y: F[(Int, Int, Int)]) => runOption(x).run == runOption(y).run

  given Scheduler =
    ExecutorServices.schedulerFromGlobalExecutionContext

}
