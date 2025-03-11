package org.atnos.example

import org.atnos.eff._
import Eff._
import Interpret._
import cats._
import cats.data._
import cats.syntax.all._

object ConsoleEffect {

  case class ConsoleString(message: String) extends AnyVal
  type Console[A] = Writer[ConsoleString, A]

  def log[R](message: String, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) WriterEffect.tell(ConsoleString(message))
    else Monad[Eff[R, *]].pure(())

  def logThrowable[R](t: Throwable, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) logThrowable(t)
    else Monad[Eff[R, *]].pure(())

  def logThrowable[R](t: Throwable)(implicit m: Member[Console, R]): Eff[R, Unit] =
    log(t.getMessage, doIt = true)(using m) >>
      log(t.getStackTrace.mkString("\n"), doIt = true) >>
      (if (t.getCause != null) logThrowable(t.getCause)
       else Monad[Eff[R, *]].pure(()))

  /**
   * This interpreter prints messages to the console
   */
  def runConsole[R, U, A](w: Eff[R, A])(implicit m: Member.Aux[Console, R, U]): Eff[U, A] =
    runConsoleToPrinter(m => println(m))(w)

  /**
   * This interpreter prints messages to a printing function
   */
  def runConsoleToPrinter[R, U, A](printer: String => Unit)(w: Eff[R, A])(implicit m: Member.Aux[Console, R, U]) =
    recurse(w)(new Recurser[Console, U, A, A] {
      def onPure(a: A) = a
      def onEffect[X](cx: Console[X]): X Either Eff[U, A] =
        cx.run match {
          case (c, x) =>
            printer(c.message)
            Left(x)
        }

      def onApplicative[X, T[_]: Traverse](ws: T[Console[X]]): T[X] Either Console[T[X]] =
        Left(ws.map { w =>
          val (c, x) = w.run
          printer(c.message)
          x
        })
    })

}
