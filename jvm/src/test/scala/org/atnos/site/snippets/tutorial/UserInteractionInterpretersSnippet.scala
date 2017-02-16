// 8<---
package org.atnos.site.snippets.tutorial

import UserInteractionSnippet._
import cats._
import cats.implicits._

trait UserInteractionInterpretersSnippet {
def readLine(): String = "snuggles"
// 8<---
import org.atnos.eff._, interpret._

def runInteract[R, A](effects: Eff[R, A])(implicit m: Interact <= R): Eff[m.Out, A] = {
  val recurse = new Recurse[Interact, m.Out, A] {
    def apply[X](i: Interact[X]): X Either Eff[m.Out, A] = Left {
      i match {
        case Ask(prompt) =>
          println(prompt)
          readLine()

        case Tell(msg) =>
          println(msg)
      }
    }

    def applicative[X, T[_] : Traverse](ms: T[Interact[X]]): T[X] Either Interact[T[X]] =
      Left(ms.map {
        case Ask(prompt) => println(prompt); readLine()
        case Tell(msg)   => println(msg)
      })

  }
  interpret1[R, m.Out, Interact, A, A](a => a)(recurse)(effects)(m)
}

def runDataOp[R, A](effects: Eff[R, A])(implicit m: DataOp <= R): Eff[m.Out, A] = {
  val memDataSet = new scala.collection.mutable.ListBuffer[String]

  val recurse = new Recurse[DataOp, m.Out, A] {
    def apply[X](i: DataOp[X]): X Either Eff[m.Out, A] = Left {
      i match {
        case AddCat(a)    => memDataSet.append(a); ()
        case GetAllCats() => memDataSet.toList
      }
    }

    def applicative[X, T[_]: Traverse](ms: T[DataOp[X]]): T[X] Either DataOp[T[X]] =
      Left(ms.map {
        case AddCat(a)    => memDataSet.append(a); ()
        case GetAllCats() => memDataSet.toList
      })

  }
  interpret1[R, m.Out, DataOp, A, A](a => a)(recurse)(effects)(m)
}

  // 8<---
}

object UserInteractionInterpretersSnippet extends UserInteractionInterpretersSnippet
