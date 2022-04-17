package user

import org.atnos.eff._
import org.atnos.eff.all._
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

}
