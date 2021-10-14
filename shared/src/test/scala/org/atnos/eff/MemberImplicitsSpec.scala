package org.atnos.eff

import org.specs2.Specification

class MemberImplicitsSpec extends Specification { def is = s2"""

  tests for member implicits

"""
/*
import cats.data._
import org.atnos.eff.syntax.all._

// UNCOMMENT TO TEST COMPILATION TIMES AND IMPLICIT SEARCH
  sealed trait OptionN[A] { def a: A }
  case class Option1[A](a: A)   extends OptionN[A]
  case class Option2[A](a: A)   extends OptionN[A]
  case class Option3[A](a: A)   extends OptionN[A]
  case class Option4[A](a: A)   extends OptionN[A]
  case class Option5[A](a: A)   extends OptionN[A]
  case class Option6[A](a: A)   extends OptionN[A]
  case class Option7[A](a: A)   extends OptionN[A]
  case class Option8[A](a: A)   extends OptionN[A]
  case class Option9[A](a: A)   extends OptionN[A]
  case class Option10[A](a: A)  extends OptionN[A]
  case class Option11[A](a: A)  extends OptionN[A]
  case class Option12[A](a: A)  extends OptionN[A]
  case class Option13[A](a: A)  extends OptionN[A]
  case class Option14[A](a: A)  extends OptionN[A]
  case class Option15[A](a: A)  extends OptionN[A]
  case class Option16[A](a: A)  extends OptionN[A]
  case class Option17[A](a: A)  extends OptionN[A]
  case class Option18[A](a: A)  extends OptionN[A]
  case class Option19[A](a: A)  extends OptionN[A]
  case class Option20[A](a: A)  extends OptionN[A]

  implicit class RunNOps[T[_] <: OptionN[_], R, A](e: Eff[R, A]) {
    def runN[U](implicit m: Member.Aux[T, R, U]): Eff[U, A] =
      e match {
        case Pure(a, _) => Eff.pure[U, A](a)
        case Impure(u, c, _) => Eff.pure[U, A](m.project(u).toOption.get.asInstanceOf[A])
        case ap@ImpureAp(unions, map, _) => new RunNOps(ap.toMonadic).runN
      }
  }

  def option1[R](implicit m: Option1 |= R): Eff[R, Int] = Eff.send(Option1(1))
  def option2[R](implicit m: Option2 |= R): Eff[R, Int] = Eff.send(Option2(1))
  def option3[R](implicit m: Option3 |= R): Eff[R, Int] = Eff.send(Option3(1))
  def option4[R](implicit m: Option4 |= R): Eff[R, Int] = Eff.send(Option4(1))
  def option5[R](implicit m: Option5 |= R): Eff[R, Int] = Eff.send(Option5(1))
  def option6[R](implicit m: Option6 |= R): Eff[R, Int] = Eff.send(Option6(1))
  def option7[R](implicit m: Option7 |= R): Eff[R, Int] = Eff.send(Option7(1))
  def option8[R](implicit m: Option8 |= R): Eff[R, Int] = Eff.send(Option8(1))
  def option9[R](implicit m: Option9 |= R): Eff[R, Int] = Eff.send(Option9(1))

  def option10[R](implicit m: Option10 |= R): Eff[R, Int] = Eff.send(Option10(1))
  def option11[R](implicit m: Option11 |= R): Eff[R, Int] = Eff.send(Option11(1))
  def option12[R](implicit m: Option12 |= R): Eff[R, Int] = Eff.send(Option12(1))
  def option13[R](implicit m: Option13 |= R): Eff[R, Int] = Eff.send(Option13(1))
  def option14[R](implicit m: Option14 |= R): Eff[R, Int] = Eff.send(Option14(1))
  def option15[R](implicit m: Option15 |= R): Eff[R, Int] = Eff.send(Option15(1))
  def option16[R](implicit m: Option16 |= R): Eff[R, Int] = Eff.send(Option16(1))
  def option17[R](implicit m: Option17 |= R): Eff[R, Int] = Eff.send(Option17(1))
  def option18[R](implicit m: Option18 |= R): Eff[R, Int] = Eff.send(Option18(1))
  def option19[R](implicit m: Option19 |= R): Eff[R, Int] = Eff.send(Option19(1))
  def option20[R](implicit m: Option20 |= R): Eff[R, Int] = Eff.send(Option20(1))


  type S1 = Fx.fx1[Option1]

  implicitly[Member[Option1, S1]]
  option1[S1].runN

  type S2 = Fx.fx2[Option1, Option2]

  implicitly[Member[Option1, S2]]
  implicitly[Member[Option2, S2]]

  option1[S2].runN.runN
  option2[S2].runN.runN

  type S3 = Fx.fx3[Option1, Option2, Option3]

  implicitly[Member[Option1, S3]]
  implicitly[Member[Option2, S3]]
  implicitly[Member[Option3, S3]]

  option1[S3].runN.runN.runN
  option2[S3].runN.runN.runN
  option3[S3].runN.runN.runN

  type S4 = Fx.fx4[Option1, Option2, Option3, Option4]

  implicitly[Member[Option1, S4]]
  implicitly[Member[Option2, S4]]
  implicitly[Member[Option3, S4]]
  implicitly[Member[Option4, S4]]

  option1[S4].runN.runN.runN.runN
  option2[S4].runN.runN.runN.runN
  option3[S4].runN.runN.runN.runN
  option4[S4].runN.runN.runN.runN

  type S5 = Fx.fx5[Option1, Option2, Option3, Option4, Option5]

  implicitly[Member[Option1, S5]]
  implicitly[Member[Option2, S5]]
  implicitly[Member[Option3, S5]]
  implicitly[Member[Option4, S5]]
  implicitly[Member[Option5, S5]]

  option1[S5].runN.runN.runN.runN.runN
  option2[S5].runN.runN.runN.runN.runN
  option3[S5].runN.runN.runN.runN.runN
  option4[S5].runN.runN.runN.runN.runN
  option5[S5].runN.runN.runN.runN.runN

  type S20 =
  FxAppend[
    FxAppend[
      Fx2[Option1, Option2],
      FxAppend[
        Fx3[Option3, Option4, Option5],
        Fx3[Option6, Option7, Option8]
        ]
      ],
    FxAppend[
      FxAppend[
        Fx3[Option9, Option10, Option11],
        Fx3[Option12, Option13, Option14]
        ],
      FxAppend[
        Fx3[Option15, Option16, Option17],
        Fx3[Option18, Option19, Option20]
        ]
      ]
    ]

  implicitly[Member[Option1, S20]]
  implicitly[Member[Option2, S20]]
  implicitly[Member[Option3, S20]]
  implicitly[Member[Option4, S20]]
  implicitly[Member[Option5, S20]]
  implicitly[Member[Option6, S20]]
  implicitly[Member[Option7, S20]]
  implicitly[Member[Option8, S20]]
  implicitly[Member[Option9, S20]]
  implicitly[Member[Option10, S20]]
  implicitly[Member[Option11, S20]]
  implicitly[Member[Option12, S20]]
  implicitly[Member[Option13, S20]]
  implicitly[Member[Option14, S20]]
  implicitly[Member[Option15, S20]]
  implicitly[Member[Option16, S20]]
  implicitly[Member[Option17, S20]]
  implicitly[Member[Option18, S20]]
  implicitly[Member[Option19, S20]]
  implicitly[Member[Option20, S20]]

  option1[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option2[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option3[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option4[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option5[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option6[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option7[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option6[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option9[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option10[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option11[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option12[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option13[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option14[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option15[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option16[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option17[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option18[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option19[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN
  option20[S20].runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN.runN


  trait R1

  type SP = Fx.prepend[Option1, R1]

  implicitly[Member[Option1, SP]]

  option1[SP].runN

  type SP2 = Fx.prepend[Option1, Fx.fx1[Option2]]

  implicitly[Member[Option1, SP2]]
  implicitly[Member[Option2, SP2]]

  option1[SP2].runN.runN
  option2[SP2].runN.runN

  type SP3 = Fx.prepend[Option1, Fx.prepend[Option2, Fx.fx1[Option3]]]

  implicitly[Member[Option1, SP3]]
  implicitly[Member[Option2, SP3]]
  implicitly[Member[Option3, SP3]]

  option1[SP3].runN.runN.runN
  option2[SP3].runN.runN.runN
  option3[SP3].runN.runN.runN

  type SD1 = FxAppend[
    Fx.fx1[Option1],
    NoFx
    ]

  implicitly[Member[Option1, SD1]]

  option1[SD1].runN

  type SDD1 = FxAppend[
    FxAppend[Fx.fx1[Option1],
             NoFx],
    NoFx
    ]

  implicitly[Member[Option1, SDD1]]

  option1[SDD1].runN

  type SD3 = FxAppend[
      Fx.fx2[Option1, Option2],
      Fx.fx1[Option3]
    ]

  implicitly[Member[Option1, SD3]]
  implicitly[Member[Option2, SD3]]
  implicitly[Member[Option3, SD3]]

  option1[SD3].runN.runN.runN
  option2[SD3].runN.runN.runN
  option3[SD3].runN.runN.runN
*/
  // PERMUTATIONS OF 4 effects
  import cats.data._

  type StateString[A] = State[String, A]
  type ReaderString[A] = Reader[String, A]
  type ValidateString[A] = Validate[String, A]

  type C = Fx.fx4[ReaderString, StateString, Choose, ValidateString]

  type Check[A] = Eff[C,A]

  val value : Check[Int] = Eff.pure(3)
  import org.atnos.eff.syntax.all._
//
  val c1 = value.runChoose[Vector].runReader("foo").runState("baz").runNel.run
  val c2 = value.runChoose[Vector].runReader("foo").runNel.runState("baz").run
  val c3 = value.runChoose[Vector].runState("foo").runReader("baz").runNel.run
  val c4 = value.runChoose[Vector].runState("foo").runNel.runReader("baz").run
  val c5 = value.runChoose[Vector].runNel.runState("foo").runReader("baz").run
  val c6 = value.runChoose[Vector].runNel.runReader("baz").runState("foo").run

  val r1 = value.runReader("foo").runChoose[Vector].runState("baz").runNel.run
  val r2 = value.runReader("foo").runChoose[Vector].runNel.runState("baz").run
  val r3 = value.runReader("foo").runState("foo").runChoose[Vector].runNel.run
  val r4 = value.runReader("foo").runState("foo").runNel.runChoose[Vector].run
  val r5 = value.runReader("foo").runNel.runState("foo").runChoose[Vector].run
  val r6 = value.runReader("foo").runNel.runChoose[Vector].runState("foo").run

  val s1 = value.runState("baz").runChoose[Vector].runReader("foo").runNel.run
  val s2 = value.runState("baz").runChoose[Vector].runNel.runReader("foo").run
  val s3 = value.runState("baz").runReader("foo").runChoose[Vector].runNel.run
  val s4 = value.runState("baz").runReader("foo").runNel.runChoose[Vector].run
  val s5 = value.runState("baz").runNel.runReader("foo").runChoose[Vector].run
  val s6 = value.runState("baz").runNel.runChoose[Vector].runReader("foo").run

  val n1 = value.runNel.runChoose[Vector].runReader("foo").runState("baz").run
  val n2 = value.runNel.runChoose[Vector].runState("baz").runReader("foo").run
  val n3 = value.runNel.runReader("foo").runChoose[Vector].runState("baz").run
  val n4 = value.runNel.runReader("foo").runState("baz").runChoose[Vector].run
  val n5 = value.runNel.runState("baz").runReader("foo").runChoose[Vector].run
  val n6 = value.runNel.runState("baz").runChoose[Vector].runReader("foo").run



  // APPEND to an arbitrary stack

  def action[R]: Eff[R, Int] =
    ???

  def actionS1[S] = action[Fx.append[Fx3[Option, Either[Throwable, *], Reader[String, *]], S]].runOption.runEither.runReader("foo")
  def actionS2[S] = action[Fx.append[Fx3[Option, Either[Throwable, *], Reader[String, *]], S]].runOption.runReader("foo").runEither
  def actionS3[S] = action[Fx.append[Fx3[Option, Either[Throwable, *], Reader[String, *]], S]].runEither.runReader("foo").runOption
  def actionS4[S] = action[Fx.append[Fx3[Option, Either[Throwable, *], Reader[String, *]], S]].runEither.runOption.runReader("foo")
  def actionS5[S] = action[Fx.append[Fx3[Option, Either[Throwable, *], Reader[String, *]], S]].runReader("foo").runEither.runOption
  def actionS6[S] = action[Fx.append[Fx3[Option, Either[Throwable, *], Reader[String, *]], S]].runReader("foo").runOption.runEither

  type SAppend[S] = Fx.append[Fx.fx4[Option, Either[Throwable, *], Reader[String, *], State[Int, *]], S]
  def actionA1[S] = action[SAppend[S]].runState(1).runOption.runEither.runReader("foo")
  def actionA2[S] = action[SAppend[S]].runState(1).runOption.runReader("foo").runEither
  def actionA3[S] = action[SAppend[S]].runState(1).runEither.runReader("foo").runOption
  def actionA4[S] = action[SAppend[S]].runState(1).runEither.runOption.runReader("foo")
  def actionA5[S] = action[SAppend[S]].runState(1).runReader("foo").runEither.runOption
  def actionA6[S] = action[SAppend[S]].runState(1).runReader("foo").runOption.runEither
  def actionA7    = action[SAppend[NoFx]].runState(1).runReader("foo").runOption.runEither


  import org.atnos.eff.option._

  def withOptionEffect[S: _Option]: Eff[S, Unit] = ???
  def withOptionEffect2[S: _Option]: Eff[S, Unit] =
    withOptionEffect[Fx.prepend[Reader[String, *], S]].runReader("hello")

  type _OptionInOut[R] = Option /= R
  type Fx1AppendFx6 = Fx.append[
    Fx.fx1[Option],
    Fx.fx6[List, Writer[String, *], Validate[String, *], Reader[String, *], State[Int, *], Either[Throwable, *]]
  ]

  def withOptionInOutEffect[S: _OptionInOut]: Eff[S, Unit] = ???
  def fx1AppendFx6Action =
    withOptionInOutEffect[Fx1AppendFx6].runOption
}
