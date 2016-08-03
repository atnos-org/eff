package org.atnos.eff

import cats.data.Xor
import org.specs2.Specification
import org.specs2.matcher.ThrownExpectations
import syntax.eff._

class IntoPolySpec extends Specification with ThrownExpectations { def is = s2"""

 The Into typeclass is used to inject the effects of one stack into another stack containing at least the same effects
   a stack can be injected into itself $into1
   a stack can be injected into another containing one prepended effect $into2

   more examples $more

"""

  sealed trait OptionLike[A] { def a: A }
  case class Option1[A](a: A) extends OptionLike[A]
  case class Option2[A](a: A) extends OptionLike[A]
  case class Option3[A](a: A) extends OptionLike[A]
  case class Option4[A](a: A) extends OptionLike[A]
  case class Option5[A](a: A) extends OptionLike[A]
  case class Option6[A](a: A) extends OptionLike[A]
  case class Option7[A](a: A) extends OptionLike[A]

  type S1 = Option1 |: NoEffect
  type S2 = Option1 |:: Option2
  type S3 = Option1 |:  Option2 |:: Option3
  type S4 = Option1 |:  Option2 |:  Option3 |:: Option4
  type S5 = Option1 |:  Option2 |:  Option3 |:  Option4 |:: Option5
  type S6 = Option1 |:  Option2 |:  Option3 |:  Option4 |:  Option5 |:: Option6
  type S7 = Option1 |:  Option2 |:  Option3 |:  Option4 |:  Option5 |:  Option6 |:: Option7


  def into1 =
    Eff.send[Option1, Fx1[Option1], Int](Option1(1)).into[Fx1[Option1]].runOpt.run === 1

  def into2 = {
    Eff.send[Option1, Fx1[Option1], Int](Option1(1)).map(identity _).into[Fx2[Option1, Option2]].runOpt.runOpt.run === 1
    Eff.send[Option2, Fx1[Option2], Int](Option2(1)).map(identity _).into[Fx2[Option1, Option2]].runOpt.runOpt.run === 1
  }

  def more = {
    Eff.send[Option1, Fx2[Option2, Option1], Int](Option1(1)).map(identity _).into[Fx2[Option1, Option2]].runOpt.runOpt.run === 1

    Eff.send[Option1, Fx1[Option1], Int](Option1(1)).map(identity _).into[Fx3[Option1, Option2, Option3]].runOpt.runOpt.runOpt.run === 1
    Eff.send[Option2, Fx1[Option2], Int](Option2(1)).map(identity _).into[Fx3[Option1, Option2, Option3]].runOpt.runOpt.runOpt.run === 1
    Eff.send[Option3, Fx1[Option3], Int](Option3(1)).map(identity _).into[Fx3[Option1, Option2, Option3]].runOpt.runOpt.runOpt.run === 1

    Eff.send[Option1, Fx2[Option1, Option2], Int](Option1(1)).map(identity _).into[Fx3[Option1, Option2, Option3]].runOpt.runOpt.runOpt.run === 1

    // make sure that flatMap works
    Eff.send[Option1, Fx2[Option1, Option2], Int](Option1(1)).flatMap(i => Eff.send(Option2(i))).into[Fx3[Option1, Option2, Option3]].runOpt.runOpt.runOpt.run === 1
 }

  implicit class RunOptionOps[T[_] <: OptionLike[_], R, U](e: Eff[R, Int])(implicit m: Member.Aux[T, R, U]) {
    def runOpt: Eff[U, Int] = runOption(e)
  }

  def runOption[T[_] <: OptionLike[_], R, U](e: Eff[R, Int])(implicit m: Member.Aux[T, R, U]): Eff[U, Int] =
    e match {
      case Pure(a) => Eff.pure(a)
      case Impure(u, c) =>
        m.project(u) match {
          case Xor.Right(oa) => runOption(c(oa.a))
          case Xor.Left(u1) => Impure[U, u1.X, Int](u1, Arrs.singleton(x => runOption(c(x))))
        }
      case a@ImpureAp(_,_) => runOption(a.toMonadic)
    }
}
