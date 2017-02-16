// 8<---
package org.atnos.site.snippets.tutorial

import UserInteractionSnippet._

trait UserInteractionProgramSnippet {
// 8<---
import org.atnos.eff._

def program[R :_interact :_dataOp]: Eff[R, Unit] =
  for {
    cat  <- askUser("What's the kitty's name?")
    _    <- addCat(cat)
    cats <- getAllCats
    _    <- tellUser("Current cats: "+cats.mkString(", "))
  } yield ()
// 8<---
}

object UserInteractionProgramSnippet extends UserInteractionProgramSnippet
