// 8<---
package org.atnos.site.snippets.tutorial

import UserInteractionSnippet._

trait UserInteractionInterpretersSnippet {
def readLine(): String = "snuggles"
// 8<---
import org.atnos.eff._, interpret._
import cats.data._


def runInteract[R, A](effects: Eff[R, A])(implicit m: Interact <= R): Eff[m.Out, A] = {
  val recurse = new Recurse[Interact, m.Out, A] {
    def apply[X](i: Interact[X]): X Xor Eff[m.Out, A] = Xor.left {
      i match {
        case Ask(prompt) =>
          println(prompt)
          readLine()

        case Tell(msg) =>
          println(msg)
      }
    }
  }
  interpret1[R, m.Out, Interact, A, A](a => a)(recurse)(effects)(m)
}

def runDataOp[R, A](effects: Eff[R, A])(implicit m: DataOp <= R): Eff[m.Out, A] = {
  val memDataSet = new scala.collection.mutable.ListBuffer[String]

  val recurse = new Recurse[DataOp, m.Out, A] {
    def apply[X](i: DataOp[X]): X Xor Eff[m.Out, A] = Xor.left {
      i match {
        case AddCat(a)    => memDataSet.append(a); ()
        case GetAllCats() => memDataSet.toList
      }
    }
  }
  interpret1[R, m.Out, DataOp, A, A](a => a)(recurse)(effects)(m)
}

  // 8<---
}

object UserInteractionInterpretersSnippet extends UserInteractionInterpretersSnippet
