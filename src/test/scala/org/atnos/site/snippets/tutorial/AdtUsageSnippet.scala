// 8<---
package org.atnos.site.snippets.tutorial

import AdtCreationSnippet._

trait AdtUsageSnippet {
// 8<---

  import org.atnos.eff._

  def program[R: _kvstore]: Eff[R, Option[Int]] =
    for {
      _ <- store("wild-cats", 2)
      _ <- update[Int, R]("wild-cats", _ + 12)
      _ <- store("tame-cats", 5)
      n <- find[Int, R]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

  // 8<---
}

object AdtUsageSnippet extends AdtUsageSnippet
