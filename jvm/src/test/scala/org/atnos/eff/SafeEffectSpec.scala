package org.atnos.eff

import cats.Eval
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.matcher.{Matcher, ThrownExpectations}
import org.scalacheck.Gen
import EitherEffect.{right => rightEffect, left => leftEffect}

import scala.collection.mutable.ListBuffer

class SafeEffectSpec extends Specification with ScalaCheck with ThrownExpectations { def is = isolated ^ s2"""

  The Safe effect can be used to protect resources and computations
  in the presence of exceptions.

  A protected action can be executed and will return a Throwable Either A $safe1

  An "attempted" action will return an exception inside the same stack if it fails $attempt1
  An "attempted" action does not side-effect                                       $attempt2

  It is possible to add a "finalizer" which will be executed whether an action is successful or not
  All the finalizers must be executed and their possible exceptions returned $finalize1

  A finalizer must be executed after the full effect has been evaluated $finalize2

  Exceptions can be caught and recovered
    with no finalizer                 $catchThrowable1
    with a finalizer before the catch $catchThrowable2
    with a finalizer after the catch  $catchThrowable3

  An exception can simply be ignored $ignoreException1

  Only particular exceptions can be caught and recovered $whenThrowable1

  Safe effect can be mixed with other effects $mixedWithOtherEffects1

  The Safe effect can be memoized $memoizeSafe
  Failed safe effects must not be memoized $dontMemoizeFailedSafe

  Finalization will happen even if other effects are involved
    either right + ok $bracket1
    either right + protect exception $bracket2
    either left + ok $bracket3
    either left + protect exception $bracket4
    it works even with flatMapped eithers $bracket5

"""

  type S = Fx.fx1[Safe]

  def safe1 = prop { n: Int =>
    protect[S, Int](action(n)).runSafe.run ====
      Either.cond(isEven(n), n, boom) -> List()
  }

  def attempt1 = prop { n: Int =>
    protect[S, Int](action(n)).attempt.runSafe.run ====
      (Right(Either.cond(isEven(n), n, boom)) -> List())
  }

  def attempt2 = {
    var evaluated = false
    protect[S, Unit]({evaluated = true}).attempt
    evaluated must beFalse
  }

  def finalize1 = prop { (n1: Int, n2: Int, n3: Int, n4: Int) =>

    val messages: ListBuffer[String] = new ListBuffer

    def program =
      for {
        a <- protect[S, Int](action(n1)) `finally` protect(unitAction { messages.append("finalizer1"); n3 })
        b <- protect[S, Int](action(n2)) `finally` protect(unitAction { messages.append("finalizer2"); n4 })
      } yield a + b

    val result = program.runSafe.run

    result match {
      case (Right(r), ls) =>
        n1 must beEven
        n2 must beEven
        r ==== (n1 + n2)

        if (isEven(n3) && isEven(n4)) ls must beEmpty
        else if (isOdd(n3) && isEven(n4)) ls must haveSize(1)
        else if (isEven(n3) && isOdd(n4)) ls must haveSize(1)
        else ls must haveSize(2)

        messages.toList ==== List("finalizer1", "finalizer2")

      case (Left(t), ls) =>
        (n1 must beOdd) or (n2 must beOdd)

        if (isEven(n1)) {

          if (isEven(n3) && isEven(n4)) ls must beEmpty
          else if (isOdd(n3) && isEven(n4)) ls must haveSize(1)
          else if (isEven(n3) && isOdd(n4)) ls must haveSize(1)
          else ls must haveSize(2)

          messages.toList ==== List("finalizer1", "finalizer2")
        }
        else {
          if (isEven(n3)) ls must beEmpty
          else ls must haveSize(1)

          // the second finalizer is not executed since the second action is not executed
          messages.toList ==== List("finalizer1")
        }

      case _ => ok
    }

  }.setGens(genInt, genInt, genInt, genInt).noShrink

  def finalize2 = prop { xs: List[Int] =>
    val messages: ListBuffer[String] = new ListBuffer

    val program =
      List(0, 2, 4).traverse(n => protect[S, Int](action(n))).void `finally` protect[S, Unit](messages.append("out"))

    program.runSafe.run
    messages.toList ==== List("out")
  }

  def catchThrowable1 = prop { n: Int =>
    val program = protect[S, Int](action(n)).catchThrowable(identity, _ => pure(1))

    program.runSafe.run ==== (Right(if (isEven(n)) n else 1) -> List())
  }.setGen(genInt)

  def catchThrowable2 = prop { n: Int =>
    val program = (protect[S, Int](action(n)) `finally` protect[S, Unit](throw finalBoom)).catchThrowable(identity, _ => pure(1))

    program.runSafe.run ==== (Right(if (isEven(n)) n else 1) -> List(finalBoom))
  }.setGen(genInt)

  def catchThrowable3 = prop { n: Int =>
    val program = protect[S, Int](action(n)).catchThrowable(identity, _ => pure(1)) `finally` protect[S, Unit](throw finalBoom)

    program.runSafe.run ==== (Right(if (isEven(n)) n else 1) -> List(finalBoom))
  }.setGen(genInt)

  def ignoreException1 = prop { n: Int =>
    def runWithException(t: Throwable): Throwable Either Unit =
      protect[S, Int](throw t).ignoreException[IllegalArgumentException].execSafe.run

    runWithException(boom) ==== Left(boom)
    runWithException(new IllegalArgumentException("ok")) must beRight[Unit]
  }

  def whenThrowable1 = prop { n: Int =>
    def runWithException(t: Throwable): Throwable Either Int =
      protect[S, Int](throw t).whenThrowable {
        case _: IllegalArgumentException => pure(0)
        case _: IllegalStateException => pure(1)
      }.execSafe.run

    runWithException(boom) ==== Left(boom)
    runWithException(new IllegalArgumentException("ok")) ==== Right(0)
    runWithException(new IllegalStateException("ok")) ==== Right(1)
  }

  def mixedWithOtherEffects1 = prop { n: Int =>

    type SOE = Fx.fx3[Safe, Option, Eval]

    def filter(x: Int): Option[Int] = if (x == 10) None else Some(x)

    def action(x: Int): Int =
      if (x == 3) throw new IllegalArgumentException
      else if (x == 5) throw new IllegalStateException("too odd")
      else x

    val program = for {
      dn <- EvalEffect.delay[SOE, Int](n)
      fdn <- OptionEffect.fromOption[SOE, Int](filter(dn))
      pdn <- protect[SOE, Int](action(fdn)).whenThrowable {
        case _: IllegalArgumentException => pure(-1)
      }
    } yield pdn

    val result = program.runEval.execSafe.runOption.run
    result match {
      case None =>
        n ==== 10
      case Some(Left(ise: IllegalStateException)) =>
        n ==== 5
        ise.getMessage ==== "too odd"
      case Some(Right(-1)) =>
        n ==== 3
      case Some(Right(x)) =>
        n ==== x
      case _ => ok
    }
  }.setGen(genInt)

  def memoizeSafe = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx1[Safe]

    val intSafe: Eff[S, Int] =
      protect[S, String]("a").flatMap(_ => protect[S, Int] { invocationsNumber += 1; 1 })

    def makeRequest: Eff[S, Int] = safeMemo("safe", cache, intSafe)

    Eff.sequenceA(List.fill(5)(makeRequest)).execSafe.run must beRight(List.fill(5)(1))

    // this should be cached as well
    makeRequest.execSafe.run must beRight(1)

    invocationsNumber must be_==(1)
  }

  def dontMemoizeFailedSafe = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx1[Safe]

    val intSafe: Eff[S, Int] =
      protect[S, String]("a").flatMap(_ => protect[S, Int] { invocationsNumber += 1; 1 })

    var firstTime = true

    def makeRequest: Eff[S, Int] =
      if (firstTime)
        safeMemo("safe", cache, SafeEffect.protect { firstTime = false; throw new Exception("boom") } >> intSafe)
      else
        safeMemo("safe", cache, intSafe)

    Eff.sequenceA(List.fill(5)(makeRequest)).execSafe.run must beLeft
    Eff.sequenceA(List.fill(5)(makeRequest)).execSafe.run must beRight(List.fill(5)(1))

    invocationsNumber must be_==(1)
  }

  var i = 0

  def bracket1 = checkRelease {
    rightEffect[U, String, Int](1) >>= (v => protect[U, Int](v))
  }

  def bracket2 = checkRelease {
    rightEffect[U, String, Int](1) >>= (v => protect[U, Int] { sys.error("ouch"); v })
  }

  def bracket3 = checkRelease {
    leftEffect[U, String, Int]("Error") >>= (v => protect[U, Int](v))
  }

  def bracket4 = checkRelease {
    leftEffect[U, String, Int]("Error") >>= (v => protect[U, Int] { sys.error("ouch"); v })
  }

  def bracket5 = checkRelease {
    rightEffect[U, String, Int](1).flatMap(_ => leftEffect[U, String, Int]("Error")) >>= (v => protect[U, Int](v))
  }

  /**
   * HELPERS
   */

  def genInt = Gen.choose(0, 10)

  def action(n: Int): Int =
    if (isEven(n)) n
    else throw boom

  def unitAction(n: Int): Unit =
    if (isEven(n)) ()
    else throw finalBoom

  val boom: Throwable = new Exception("boom")
  val finalBoom: Throwable = new Exception("finalBoom")

  def isEven(n: Int) = n % 2 == 0
  def isOdd(n: Int) = n % 2 == 1

  def beEven: Matcher[Int] = (n: Int) => (isEven(n), s"$n is not even")
  def beOdd: Matcher[Int] = (n: Int) => (isOdd(n), s"$n is not odd")

  // to check finalization with other effects
  type _eitherString[R] = Either[String, *] |= R

  def acquire[R :_Safe]: Eff[R, Int] = protect[R, Int] { i += 1; i }
  def release[R :_Safe]: Int => Eff[R, Int] = (_: Int) => protect[R, Int] { i -= 1; i }

  def checkRelease(use: Eff[U, Int]) = {
    bracketAction(use).execSafe.flatMap(either => fromEither(either.leftMap(_.getMessage))).runEither.run
    i ==== 0
  }

  def bracketAction[R :_Safe :_eitherString](use: Eff[R, Int]): Eff[R, Int] =
    bracket(acquire[R])(_ => use)(release[R])

  type U = Fx.fx2[Safe, Either[String, *]]



}
