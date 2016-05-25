package org.atnos.eff

import scala.util.control.NonFatal
import cats.data._
import Xor._
import cats.syntax.functor._
import Eff._
import Interpret._
import cats.~>
import org.atnos.eff.EvalEffect.Eval

import scala.reflect.ClassTag

/**
 * Effect for computation which can fail and return a Throwable, or just stop with a failure
 *
 * This effect is a mix of Eval and Xor in the sense that every computation passed to this effect (with the ok
 * method) is considered "impure" or "faulty" by default.
 *
 * The type F is used to represent the failure type.
 *
 */
trait ErrorEffect[F] extends
  ErrorCreation[F] with
  ErrorInterpretation[F]

trait ErrorTypes[F] {

  /** type of errors: exceptions or failure messages */
  type Error = Throwable Xor F

  /**
   * base type for this effect: either an error or a computation to evaluate
   * scala.Name represents "by-name" value: values not yet evaluated
   */
  type ErrorOrOk[A] = Error Xor cats.Eval[A]
}

trait ErrorCreation[F] extends ErrorTypes[F] {
  /** create an Eff value from a computation */
  def ok[R, A](a: => A)(implicit m: ErrorOrOk <= R): Eff[R, A] =
    send[ErrorOrOk, R, A](Right(cats.Eval.later(a)))

  /** create an Eff value from a computation */
  def eval[R, A](a: Eval[A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
    send[ErrorOrOk, R, A](Right(a))

  /** create an Eff value from an error */
  def error[R, A](error: Error)(implicit m: ErrorOrOk <= R): Eff[R, A] =
    send[ErrorOrOk, R, A](Left(error))

  /** create an Eff value from a failure */
  def fail[R, A](failure: F)(implicit m: ErrorOrOk <= R): Eff[R, A] =
    error(Right(failure))

  /** create an Eff value from an exception */
  def exception[R, A](t: Throwable)(implicit m: ErrorOrOk <= R): Eff[R, A] =
    error(Left(t))
}

trait ErrorInterpretation[F] extends ErrorCreation[F] { outer =>

  /**
   * Run an error effect.
   *
   * Stop all computation if there is an exception or a failure.
   */
  def runError[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member.Aux[ErrorOrOk, R, U]): Eff[U, Error Xor A] = {
    val recurse = new Recurse[ErrorOrOk, U, Error Xor A] {
      def apply[X](m: ErrorOrOk[X]) =
        m match {
          case Left(e) =>
            Right(EffMonad[U].pure(Left(e)))

          case Right(a) =>
            try Left(a.value)
            catch { case NonFatal(t) => Right(EffMonad[U].pure(Left(Left(t)))) }
        }
    }

    interpret1[R, U, ErrorOrOk, A, Error Xor A]((a: A) => Right(a): Error Xor A)(recurse)(r)
  }

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action whether the first is successful or not
   */
  def andFinally[R <: Effects, A](action: Eff[R, A], last: Eff[R, Unit])(implicit m: ErrorOrOk <= R): Eff[R, A] = {
    val recurse = new Recurse[ErrorOrOk, R, A] {
      def apply[X](current: ErrorOrOk[X]): X Xor Eff[R, A] =
        current match {
          case Left(e) => Right(last.flatMap(_ => outer.error[R, A](e)))
          case Right(x) =>
            try Left(x.value)
            catch { case NonFatal(t) => Right(last.flatMap(_ => outer.exception[R, A](t))) }
        }
    }
    intercept[R, ErrorOrOk, A, A]((a: A) => last.as(a), recurse)(action)
  }

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful
   */
  def orElse[R <: Effects, A](action: Eff[R, A], onError: Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
    whenFailed(action, _ => onError)

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful, based on the error
   */
  def catchError[R <: Effects, A, B](action: Eff[R, A], pure: A => B, onError: Error => Eff[R, B])(implicit m: ErrorOrOk <= R): Eff[R, B] = {
    val recurse = new Recurse[ErrorOrOk, R, B] {
      def apply[X](current: ErrorOrOk[X]): X Xor Eff[R, B] =
        current match {
          case Left(e) => Right(onError(e))
          case Right(x) =>
            try Left[X](x.value)
            catch { case NonFatal(t) => Right(onError(Left(t))) }
        }
    }
    intercept1[R, ErrorOrOk, A, B](pure)(recurse)(action)
  }

  /**
   * evaluate 1 action possibly having error effects
   *
   * Execute a second action if the first one is not successful, based on the error
   *
   * The final value type is the same as the original type
   */
  def whenFailed[R <: Effects, A](action: Eff[R, A], onError: Error => Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, A] =
    catchError(action, identity[A], onError)

  /**
   * ignore one possible exception that could be thrown
   */
  def ignoreException[R <: Effects, E <: Throwable : ClassTag, A](action: Eff[R, A])(implicit m: ErrorOrOk <= R): Eff[R, Unit] =
    catchError[R, A, Unit](action, (a: A) => (), { error: Error =>
      error match {
        case Xor.Left(t) if implicitly[ClassTag[E]].runtimeClass.isInstance(t) =>
          EffMonad[R].pure(())
        case other => outer.error(other)
      }
    })

  /**
   * Lift a computation over a "small" error (for a subsystem) into
   * a computation over a "bigger" error (for the full application)
   */
  def localError[SR, BR, U, E1, E2, A](r: Eff[SR, A], getter: E1 => E2)
                                     (implicit sr: Member.Aux[({type l[X]=(Throwable Xor E1) Xor Eval[X]})#l, SR, U],
                                               br: Member.Aux[({type l[X]=(Throwable Xor E2) Xor Eval[X]})#l, BR, U]): Eff[BR, A] =
    transform[SR, BR, U, ({type l[X]=(Throwable Xor E1) Xor Eval[X]})#l, ({type l[X]=(Throwable Xor E2) Xor Eval[X]})#l, A](r,
      new ~>[({type l[X]=(Throwable Xor E1) Xor Eval[X]})#l, ({type l[X]=(Throwable Xor E2) Xor Eval[X]})#l] {
      def apply[X](r: (Throwable Xor E1) Xor Eval[X]): (Throwable Xor E2) Xor Eval[X] =
        r.leftMap(_.map(getter))
    })

}

/**
 * Simple instantiation of the ErrorEffect trait with String as a Failure type
 */
object ErrorEffect extends ErrorEffect[String] {

  def render(t: Throwable): String =
    s"Error[${t.getClass.getName}]" + (Option(t.getMessage) match {
      case None          => ""
      case Some(message) => s" $message"
    })

  def renderWithStack(t: Throwable): String =
    s"""============================================================
       |${render(t)}
       |------------------------------------------------------------
       |${traceWithIndent(t, "    ")}
       |============================================================
       |""".stripMargin

  def trace(t: Throwable): String =  {
    val out = new java.io.StringWriter
    t.printStackTrace(new java.io.PrintWriter(out))
    out.toString
  }

  def traceWithIndent(t: Throwable, indent: String): String =
    trace(t).lines.map(line => indent + line).mkString("\n")
}
