package user

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.eval._
import org.atnos.eff.syntax.addon.scalaz.all._
import org.atnos.eff.addon.scalaz._
import cats.Eval
import scalaz._
import Scalaz._
import org.specs2._

class ScalazSpec extends Specification with Specs2Compat {
  def is = s2"""

 The Eff monad must be usable with Scalaz typeclasses
   for Monad, Applicative, Traverse $traverse
   with an Applicative traverse $applicativeTraverse

"""

  def traverse = {
    def action[R: _eval]: Eff[R, List[Int]] =
      List(1, 2, 3).traverse { i => delay(i) }

    type S = Fx1[Eval]
    action[S].runEval.run must_== List(1, 2, 3)
  }

  def applicativeTraverse = {
    def action[R: _eval]: Eff[R, List[Int]] =
      List(1, 2, 3).traverseA { i => delay(i) }

    type S = Fx1[Eval]
    action[S].runEval.run must_== List(1, 2, 3)
  }

}
