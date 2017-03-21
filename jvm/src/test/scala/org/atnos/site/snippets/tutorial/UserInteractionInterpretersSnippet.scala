// 8<---
package org.atnos.site.snippets.tutorial

import UserInteractionSnippet._
import cats._
import cats.implicits._

trait UserInteractionInterpretersSnippet {
def readLine(): String = "snuggles"
// 8<---
import org.atnos.eff._, interpret._

def runInteract[R, A](effect: Eff[R, A])(implicit m: Interact <= R): Eff[m.Out, A] =
  recurse(effect)(new Recurser[Interact, m.Out, A, A] {
    def onPure(a: A): A = a

    def onEffect[X](i: Interact[X]): X Either Eff[m.Out, A] = Left {
      i match {
        case Ask(prompt) =>
          println(prompt)
          readLine()

        case Tell(msg) =>
          println(msg)
      }
    }

    def onApplicative[X, T[_] : Traverse](ms: T[Interact[X]]): T[X] Either Interact[T[X]] =
      Left(ms.map {
        case Ask(prompt) => println(prompt); readLine()
        case Tell(msg)   => println(msg)
      })

  })(m)

def runDataOp[R, A](effect: Eff[R, A])(implicit m: DataOp <= R): Eff[m.Out, A] = {
  val memDataSet = new scala.collection.mutable.ListBuffer[String]

  recurse(effect)(new Recurser[DataOp, m.Out, A, A] {
    def onPure(a: A): A = a

    def onEffect[X](i: DataOp[X]): X Either Eff[m.Out, A] = Left {
      i match {
        case AddCat(a)    => memDataSet.append(a); ()
        case GetAllCats() => memDataSet.toList
      }
    }

    def onApplicative[X, T[_]: Traverse](ms: T[DataOp[X]]): T[X] Either DataOp[T[X]] =
      Left(ms.map {
        case AddCat(a)    => memDataSet.append(a); ()
        case GetAllCats() => memDataSet.toList
      })
  })(m)

}

  // 8<---
}

object UserInteractionInterpretersSnippet extends UserInteractionInterpretersSnippet
