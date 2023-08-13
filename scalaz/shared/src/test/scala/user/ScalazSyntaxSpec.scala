package user

import org.atnos.eff._
import org.atnos.eff.eval._
import org.atnos.eff.option._
import org.atnos.eff.syntax.eval._
import org.atnos.eff.syntax.addon.scalaz.all._
import cats.Eval
import scalaz._
import Scalaz._
import org.specs2._

class ScalazSyntaxSpec extends Specification {
  def is = s2"""

 standard operations of eff values can be called with Scalaz type classes

"""

  def run =
    delay[Fx1[Eval], Int](1).runEval.run

  def detach =
    fromOption[Fx1[Option], Int](Option(1)).detach

  def detachA =
    fromOption[Fx1[Option], Int](Option(1)).detachA(Applicative[Option])

  def traverseA =
    List(1).traverseA(i => fromOption[Fx1[Option], Int](Option(i)))

  def flatTraverseA =
    List(1).flatTraverseA(i => fromOption[Fx1[Option], List[Int]](Option(List(i))))

  def sequenceA =
    List(1).map(i => fromOption[Fx1[Option], Int](Option(i))).sequenceA

  def flatSequenceA =
    List(1).map(i => fromOption[Fx1[Option], List[Int]](Option(List(i)))).flatSequenceA

  def runValidationNel[A](eff: Eff[Fx.fx1[Validate[Int, *]], A]): scalaz.ValidationNel[Int, A] =
    eff.runValidationNel.run

  def runNelDisjunction[A](eff: Eff[Fx.fx1[Validate[Int, *]], A]): scalaz.NonEmptyList[Int] \/ A =
    eff.runNelDisjunction.run

  def runMapDisjunction[A, B: Semigroup, C](eff: Eff[Fx.fx1[Validate[A, *]], C], f: A => B): B \/ C =
    eff.runMapDisjunction(f).run

}
