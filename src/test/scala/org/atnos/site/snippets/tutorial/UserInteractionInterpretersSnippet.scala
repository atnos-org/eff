// 8<---
package org.atnos.site.snippets.tutorial

import org.atnos.site.snippets.tutorial.UserInteractionSnippet.*

trait UserInteractionInterpretersSnippet {
// 8<---
  import cats._
  import cats.syntax.all._
  import org.atnos.eff._
  import interpret._

  def readLine(): String =
    "snuggles"

  def runInteract[R, A](effect: Eff[R, A])(using m: Interact <= R): Eff[m.Out, A] =
    recurse(effect)(new Recurser[Interact, m.Out, A, A] {
      def onPure(a: A): A = a

      def onEffect[X](i: Interact[X]): Either[X, Eff[m.Out, A]] = Left[X, Eff[m.Out, A]] {
        i match {
          case Ask(prompt) =>
            println(prompt)
            readLine()

          case Tell(msg) =>
            println(msg)
        }
      }

      def onApplicative[X, T[_]: Traverse](ms: T[Interact[X]]): Either[T[X], Interact[T[X]]] =
        Left(ms.map {
          case Ask(prompt) => println(prompt); readLine()
          case Tell(msg) => println(msg)
        })

    })(using m)

  def runDataOp[R, A](effect: Eff[R, A])(using m: DataOp <= R): Eff[m.Out, A] = {
    val memDataSet = new scala.collection.mutable.ListBuffer[String]

    recurse(effect)(new Recurser[DataOp, m.Out, A, A] {
      def onPure(a: A): A = a

      def onEffect[X](i: DataOp[X]): Either[X, Eff[m.Out, A]] = Left[X, Eff[m.Out, A]] {
        i match {
          case AddCat(a) => memDataSet.append(a); ()
          case GetAllCats() => memDataSet.toList
        }
      }

      def onApplicative[X, T[_]: Traverse](ms: T[DataOp[X]]): Either[T[X], DataOp[T[X]]] =
        Left(ms.map {
          case AddCat(a) => memDataSet.append(a); ()
          case GetAllCats() => memDataSet.toList
        })
    })(using m)

  }

  // 8<---
}

object UserInteractionInterpretersSnippet extends UserInteractionInterpretersSnippet
