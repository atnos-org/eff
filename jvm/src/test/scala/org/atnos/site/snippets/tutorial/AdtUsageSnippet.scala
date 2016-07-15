// 8<---
package org.atnos.site.snippets.tutorial

import AdtSnippet._
import AdtCreationSnippet._

trait AdtUsageSnippet {
// 8<---

import org.atnos.eff._

def program[R :_kvstore]: Eff[R, Option[Int]] =
  for {
    _ <- put("wild-cats", 2)
    _ <- update[Int, R]("wild-cats", _ + 12)
    _ <- put("tame-cats", 5)
    n <- get[Int, R]("wild-cats")
    _ <- delete("tame-cats")
  } yield n

  // 8<---
}

object AdtUsageSnippet extends AdtUsageSnippet
