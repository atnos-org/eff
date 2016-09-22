package org.atnos.example

import org.atnos.eff._
import Eff._
import Interpret._
import cats._
import cats.data._
import cats.implicits._

object ConsoleEffect {

  case class ConsoleString(message: String) extends AnyVal
  type Console[A] = Writer[ConsoleString, A]

  def log[R](message: String, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) WriterEffect.tell(ConsoleString(message))
    else      EffMonad.pure(())

  def logThrowable[R](t: Throwable, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) logThrowable(t)
    else      EffMonad.pure(())

  def logThrowable[R](t: Throwable)(implicit m: Member[Console, R]): Eff[R, Unit] =
    log(t.getMessage, doIt = true)(m) >>
    log(t.getStackTrace.mkString("\n"), doIt = true) >>
      (if (t.getCause != null) logThrowable(t.getCause)
       else                    EffMonad.pure(()))


  /**
   * This interpreter prints messages to the console
   */
  def runConsole[R, U, A](w: Eff[R, A])(implicit m : Member.Aux[Console, R, U]): Eff[U, A] =
    runConsoleToPrinter(m => println(m))(w)

  /**
   * This interpreter prints messages to a printing function
   */
  def runConsoleToPrinter[R, U, A](printer: String => Unit)(w: Eff[R, A])(implicit m : Member.Aux[Console, R, U]) = {
    val recurse = new Recurse[Console, U, A] {
      def apply[X](cx: Console[X]): X Xor Eff[U, A] =
        cx.run match {
          case (c, x) =>
            printer(c.message)
            Xor.Left(x)
        }

      def applicative[X, T[_]: Traverse](ws: T[Console[X]]): T[X] Xor Console[T[X]] =
        Xor.Left(ws.map { w =>
          val (c, x) = w.run
          printer(c.message)
          x
        })
    }

    interpret1((a: A) => a)(recurse)(w)(m)
  }

}
