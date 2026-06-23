package user

import cats.Eval
import org.atnos.eff.*
import org.atnos.eff.addon.scalaz.given
import org.atnos.eff.eval.*
import org.atnos.eff.syntax.addon.scalaz.all.given
import org.atnos.eff.syntax.eval.given
import org.specs2.*
import scalaz.Scalaz.*

class ScalazSpec extends Specification {
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
