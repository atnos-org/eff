// 8<---
package org.atnos.site.snippets.tutorial

trait UserInteractionSnippet {
// 8<---
import org.atnos.eff._, all._

sealed trait Interact[A]

case class Ask(prompt: String) extends Interact[String]
case class Tell(msg: String) extends Interact[Unit]

type _Interact[R] = Member[Interact, R]

def askUser[R](prompt: String)(implicit m: Member[Interact, R]): Eff[R, String] =
  send(Ask(prompt))

def tellUser[R](message: String)(implicit m: Member[Interact, R]): Eff[R, Unit] =
  send(Tell(message))

sealed trait DataOp[A]

type _DataOp[R] = Member[DataOp, R]

case class AddCat(a: String) extends DataOp[Unit]
case class GetAllCats() extends DataOp[List[String]]

def addCat[R](a: String)(implicit m: Member[DataOp, R]): Eff[R, Unit] =
  send(AddCat(a))

def getAllCats[R](implicit m: Member[DataOp, R]): Eff[R, List[String]] =
  send(GetAllCats())


  // 8<---
}

object UserInteractionSnippet extends UserInteractionSnippet
