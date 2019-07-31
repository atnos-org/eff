package org.atnos.eff.syntax.addon

import org.atnos.eff.Eff
import org.atnos.eff.addon.zio.ZIOEffect


package object zio extends ZIOEffect {
  implicit final def toZIOOps[R, ENV, E, A](e: Eff[R, A]): ZIOOps[R, ENV, E, A] = new ZIOOps[R, ENV, E, A](e)
}
