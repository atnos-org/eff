package org.atnos.eff
package syntax.addon.scalaz

import org.atnos.eff.addon.scalaz._

object eff extends eff

trait eff extends org.atnos.eff.syntax.effOperations with effScalaz

trait effScalaz {

  implicit final def toEffScalazOneEffectOps[M[_], A](e: Eff[Fx1[M], A]): EffScalazOneEffectOps[M, A] = new EffScalazOneEffectOps(e)
  implicit final def toEffScalazApplicativeOps[F[_], A](values: F[A]): EffScalazApplicativeOps[F, A] = new EffScalazApplicativeOps(values)
  implicit final def toEffScalazSequenceOps[F[_], R, A](values: F[Eff[R, A]]): EffScalazSequenceOps[F, R, A] = new EffScalazSequenceOps(values)
  implicit final def toEffScalazFlatSequenceOps[F[_], R, A](values: F[Eff[R, F[A]]]): EffScalazFlatSequenceOps[F, R, A] = new EffScalazFlatSequenceOps(values)

}

