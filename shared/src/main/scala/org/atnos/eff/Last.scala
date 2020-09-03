package org.atnos.eff

import cats._
import cats.syntax.all._

import scala.util.control.NonFatal

/**
 * Encapsulation of one optional last action to execute at the end of the program
 */
case class Last[R](value: Option[Eval[Eff[R, Unit]]]) {

  /** interpret this last action as a set of effects in another stack */
  def interpret[U](n: Eff[R, Unit] => Eff[U, Unit]): Last[U] =
    Last[U](value.map(v => v.map(n)))

  def interpretEff[U](n: Last[R] => Eff[U, Unit]): Last[U] =
    Last.eff(n(this))

  def <*(last: Last[R]): Last[R] =
    (value, last.value) match {
      case (None, None)       => this
      case (Some(r), None)    => this
      case (None, Some(l))    => last
      case (Some(r), Some(l)) => Last(Option(r *> l))
    }

  def *>(last: Last[R]): Last[R] =
    (value, last.value) match {
      case (None, None)       => this
      case (Some(r), None)    => this
      case (None, Some(l))    => last
      case (Some(r), Some(l)) => Last(Option(r *> l))
    }
}

object Last {

  def none[R]: Last[R] =
    Last(None)

  def eff[R](e: =>Eff[R, Unit]): Last[R] =
    Last(Option(Eval.later(evaluate(e))))

  def evaluate[R](e: =>Eff[R, Unit]): Eff[R, Unit] =
    try e
    catch { case NonFatal(t) =>
       if (sys.props.isDefinedAt("eff.debuglast"))
         println("executing one last eff action failed\n"+t.getStackTrace.mkString("\n"))
        Eff.pure[R, Unit](())
    }

}
