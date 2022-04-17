package org.atnos.eff.syntax.addon.cats

import cats.effect.IO
import org.atnos.eff._

package object effect {

  implicit final def toIOOps[A](e: Eff[Fx1[IO], A]): IOOps[A] = new IOOps[A](e)
  implicit final def toIOOps2[R, A](e: Eff[R, A]): IOOps2[R, A] = new IOOps2[R, A](e)

}
