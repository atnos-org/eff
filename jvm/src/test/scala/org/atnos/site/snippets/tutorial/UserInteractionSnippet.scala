// 8<---
package org.atnos.site.snippets.tutorial

trait UserInteractionSnippet {
// 8<---
import org.atnos.eff._, all._

sealed trait Interact[A]

case class Ask(prompt: String) extends Interact[String]
case class Tell(msg: String) extends Interact[Unit]

type _interact[R] = Interact |= R

def askUser[R :_interact](prompt: String): Eff[R, String] =
  send(Ask(prompt))

def tellUser[R :_interact](message: String): Eff[R, Unit] =
  send(Tell(message))

sealed trait DataOp[A]

type _dataOp[R] = DataOp |= R

case class AddCat(a: String) extends DataOp[Unit]
case class GetAllCats() extends DataOp[List[String]]

def addCat[R :_dataOp](a: String): Eff[R, Unit] =
  send(AddCat(a))

def getAllCats[R :_dataOp]: Eff[R, List[String]] =
  send(GetAllCats())


  // 8<---
}

object UserInteractionSnippet extends UserInteractionSnippet
