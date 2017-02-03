package org.atnos.eff

import org.scalacheck.Gen.posNum
import org.specs2.{ScalaCheck, Specification}

import cats.data._
import cats.syntax.all._
import cats.instances.all._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.scalacheck.Gen

class EitherEffectSpec extends Specification with ScalaCheck { def is = s2"""

 an Either value can be injected in the stack    $EitherCreation

 run the Either effect monad                     $EitherMonad
 run the Either effect monad with nothing        $EitherWithKoMonad
 run the Either effect monad with reader         $EitherReader

 run is stack safe with Either                   $stacksafeRun
 a left value can be caught and transformed to a right value $leftToRight

 the left type can be modified with local in a different stack $local
 the left type can be run with local in the same stack         $localRun
 the left type can be modified implicitly                      $implicitRequireLeft

 exceptions can also be caught
   non fatal exceptions with a handler $handleFromCatchNonFatal
   non fatal exceptions                $catchNonFatal

 left values can be accumulated in the applicative case if they have a Semigroup instance
   when running the effect $runEitherWithCombine

 runEitherU can also run the Either effect monad  $runEitherU
 runEither can run two Either effects             $runTwoEither

"""

  def EitherCreation = prop { stringOrInt: String Either Int =>
    type S = Fx.fx1[EitherString]

    val either1 = stringOrInt
    val e: Eff[S, Int] = fromEither(either1)
    e.runEither.run ==== either1
  }

  def EitherMonad = {
    type S = Fx.fx1[EitherString]

    val either: Eff[S, Int] =
      for {
        i <- EitherEffect.right[S, String, Int](1)
        j <- EitherEffect.right[S, String, Int](2)
      } yield i + j

    either.runEither.run === Right(3)
  }

  def EitherWithKoMonad = {
    type S = Fx.fx1[EitherString]

    val either: Eff[S, Int] =
      for {
        i <- EitherEffect.right[S, String, Int](1)
        j <- EitherEffect.left[S, String, Int]("error!")
      } yield i + j

    either.runEither.run === Left("error!")
  }

  def EitherReader = prop { (init: Long, someValue: Int) =>

    // define a Reader / Either stack
    type ReaderLong[A] = Reader[Long, A]
    type S = Fx.fx2[EitherString, ReaderLong]

    // create actions
    val readEither: Eff[S, Int] =
      for {
        j <- EitherEffect.right[S, String, Int](someValue)
        i <- ask[S, Long]
      } yield i.toInt + j

    // run effects
    readEither.runEither.runReader(init).run must_==
      Right(init.toInt + someValue)

  }.setGens(posNum[Long], posNum[Int])

  type EitherString[A] = String Either A

  def stacksafeRun = {
    type E = Fx.fx1[EitherString]

    val list = (1 to 5000).toList
    val action = list.traverse(i => EitherEffect.right[E, String, String](i.toString))

    action.runEither.run ==== Right(list.map(_.toString))
  }

  def leftToRight = prop { i: Int =>
    case class TooBig(value: Int)
    type D[A] = TooBig Either A
    type E = Fx.fx1[D]

    val i = 7

    val value: Eff[E, Int] =
      if (i > 5) EitherEffect.left[E, TooBig, Int](TooBig(i))
      else       EitherEffect.right[E, TooBig, Int](i)

    val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) { case TooBig(k) =>
      if (k < 10) EitherEffect.right[E, TooBig, Int](k)
      else        EitherEffect.left[E, TooBig, Int](TooBig(k))
    }

    val expected: TooBig Either Int =
      if (i < 10) Right(i) else Left(TooBig(i))

    val actual: TooBig Either Int =
      action.runEither.run

    actual == expected

  }.setGen(Gen.oneOf(14, 12))

  def local = {
    case class Error1(m: String)

    case class Error2(e1: Error1)

    type R1 = Fx.fx1[(Error1 Either ?)]
    type R2 = Fx.fx1[(Error2 Either ?)]

    val action1: Eff[R1, Unit] =
      EitherEffect.left(Error1("boom"))

    val action2: Eff[R2, Unit] =
      action1.zoomEither(Error2)

    action2.runEither.run ==== Left(Error2(Error1("boom")))
  }

  def localRun = {
    case class Error1(m: String)
    case class Error2(e1: Error1)

    type R1 = Fx.fx2[Error1 Either ?, Error2 Either ?]

    val action1: Eff[R1, Unit] =
      EitherEffect.left(Error1("boom"))

    action1.runLocalEither(Error2).runEither.run ==== Left(Error2(Error1("boom")))
  }

  def implicitRequireLeft = {
    case class Error1(m: String)
    case class Error2(e1: Error1)
    implicit def e1Toe2: Error1 => Error2 = (e1: Error1) => Error2(e1)
    import either._

    def withE1[R](i: Int)(implicit m: (Error1 Either ?) |= R): Eff[R, Int] =
      either.right[R, Error1, Int](i)

    def withE2[R](implicit m: (Error2 Either ?) |= R): Eff[R, String] =
      withE1[R](10).map(_.toString)

    withE2[Fx.fx1[Error2 Either ?]].runEither.run ==== Right("10")
  }

  def handleFromCatchNonFatal = {
    val newException = new Exception("bam")
    val caught = EitherEffect.fromCatchNonFatal { throw new Exception("boom"); 1 } ((t: Throwable) => newException)

    caught.runEither.run must beLeft(newException)
  }

  def catchNonFatal = {
    val exception = new Exception("boom")
    val caught = EitherEffect.catchNonFatalThrowable { throw exception; 1 }

    caught.runEither.run must beLeft(exception)
  }

  def runEitherWithCombine = {
    type R = Fx1[String Either ?]
    val action =
      EitherEffect.left[R, String, Int]("a") *>
      EitherEffect.left[R, String, Int]("b") *>
      EitherEffect.left[R, String, Int]("c")

    action.runEitherCombine.run must beLeft("abc")
  }

  def runEitherU = {
    type S = Fx.fx1[EitherString]

    val either: Eff[S, Int] = fromEither(Right(3))

    either.runEitherU.run === Right(3)
  }

  def runTwoEither = {
    type EitherInt[A] = Int Either A
    type S = Fx.fx2[EitherString, EitherInt]

    val either: Eff[S, Int] = fromEither(Right[Int, Int](3))

    val e1: Either[Int, Either[String, Int]] = either.runEither[String].runEither[Int].run
    val e2: Either[String, Either[Int, Int]] = either.runEither[Int].runEither[String].run

    e1 === Right(Right(3)) && e2 === Right(Right(3))
  }
}

