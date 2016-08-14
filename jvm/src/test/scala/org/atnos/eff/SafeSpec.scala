package org.atnos.eff

import cats._
import data._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.matcher.{Matcher, ThrownExpectations}
import org.scalacheck.Gen
import org.specs2.matcher.XorMatchers._
import scala.collection.mutable.ListBuffer

class SafeSpec extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

  The Safe effect can be used to protect resources and computations
  in the presence of exceptions.

  A protected action can be executed and will return a Throwable Xor A $safe1

  An "attempted" action will return an exception inside the same stack if it fails $attempt1

  It is possible to add a "finalizer" which will be executed wether an action is successful or not
  All the finalizers must be executed and their possible exceptions returned $finalize1

  Exceptions can be caught and recovered
    with no finalizer                 $catchThrowable1
    with a finalizer before the catch $catchThrowable2
    with a finalizer after the catch  $catchThrowable3

  An exception can simply be ignored $ignoreException1

"""

  type S = Fx.fx2[Safe, Option]

  def safe1 = prop { n: Int =>
    protect[S, Int](action(n)).runSafe.runOption.run ====
      (if (isEven(n)) Option((Xor.right(n), List()))
       else           Option((Xor.left(boom), List())))
  }

  def attempt1 = prop { n: Int =>
    protect[S, Int](action(n)).attempt.runSafe.runOption.run ====
      Option(
        (Xor.right(if (isEven(n)) Xor.right(n) else Xor.left(boom)),
         List())
      )
  }

  def finalize1 = prop { (n1: Int, n2: Int, n3: Int, n4: Int) =>
    val messages: ListBuffer[String] = new ListBuffer

    def program =
      for {
        a <- protect[S, Int](action(n1)) `finally` protect(unitAction { messages.append("finalizer1"); n3 })
        b <- protect[S, Int](action(n2)) `finally` protect(unitAction { messages.append("finalizer2"); n4 })
      } yield a + b

    val result = program.runSafe.runOption.run

    result match {
      case Some((Xor.Right(r), ls)) =>
        n1 must beEven
        n2 must beEven
        r ==== (n1 + n2)

        if (isEven(n3) && isEven(n4)) ls must beEmpty
        else if (isOdd(n3) && isEven(n4)) ls must haveSize(1)
        else if (isEven(n3) && isOdd(n4)) ls must haveSize(1)
        else ls must haveSize(2)

        messages.toList ==== List("finalizer1", "finalizer2")

      case Some((Xor.Left(t), ls)) =>
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

      case None => ok
    }

  }.setGens(genInt, genInt, genInt, genInt).noShrink

  def catchThrowable1 = prop { n: Int =>
    val program = protect[S, Int](action(n)).catchThrowable(identity, _ => pure(1))

    program.runSafe.runOption.run ==== Option((Xor.Right(if (isEven(n)) n else 1), List()))
  }.setGen(genInt)

  def catchThrowable2 = prop { n: Int =>
    val program = (protect[S, Int](action(n)) `finally` protect[S, Unit](throw finalBoom)).catchThrowable(identity, _ => pure(1))

    program.runSafe.runOption.run ==== Option((Xor.Right(if (isEven(n)) n else 1), List(finalBoom)))
  }.setGen(genInt)

  def catchThrowable3 = prop { n: Int =>
    val program = protect[S, Int](action(n)).catchThrowable(identity, _ => pure(1)) `finally` protect[S, Unit](throw finalBoom)

    program.runSafe.runOption.run ==== Option((Xor.Right(if (isEven(n)) n else 1), List(finalBoom)))
  }.setGen(genInt)

  def ignoreException1 = prop { n: Int =>
    def runWithException(t: Throwable): Option[Throwable Xor Unit] =
      protect[S, Int](throw t).ignoreException[IllegalArgumentException].execSafe.runOption.run

    runWithException(boom) must beSome(beXorLeft[Throwable])
    runWithException(new IllegalArgumentException("ok")) must beSome(beXorRight[Unit])
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
    else throw boom

  val boom: Throwable = new Exception("boom")
  val finalBoom: Throwable = new Exception("finalBoom")

  def isEven(n: Int) = n % 2 == 0
  def isOdd(n: Int) = n % 2 == 1

  def beEven: Matcher[Int] = (n: Int) => (isEven(n), s"$n is not even")
  def beOdd: Matcher[Int] = (n: Int) => (isOdd(n), s"$n is not odd")

}
