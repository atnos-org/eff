package org.atnos.eff

import cats.*
import org.atnos.eff.eff.*

trait SafeCreation extends SafeTypes {

  def protect[R: _safe, A](a: => A): Eff[R, A] =
    send[Safe, R, A](Safe.evaluate(a))

  def eval[R: _safe, A](a: Eval[A]): Eff[R, A] =
    send[Safe, R, A](Safe.eval(a))

  def exception[R: _safe, A](t: Throwable): Eff[R, A] =
    send[Safe, R, A](Safe.fail(t))

  def finalizerException[R: _safe](t: Throwable): Eff[R, Unit] =
    send[Safe, R, Unit](Safe.failFinalizer(t))
}
