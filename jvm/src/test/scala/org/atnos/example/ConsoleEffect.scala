package org.atnos.example

import org.atnos.eff._
import Eff._
import Interpret._
import cats.syntax.all._
import cats.data._
import Tag._

object ConsoleEffect {

  trait ConsoleTag

  type Console[A] = Writer[String, A] @@ ConsoleTag

  def log[R](message: String, doIt: Boolean = true)(implicit m: Member[Console, R]): Eff[R, Unit] =
    if (doIt) WriterEffect.tell(message)(Member.untagMember[Writer[String, ?], R, ConsoleTag](m))
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
  def runConsole[R <: Effects, U <: Effects, A](w: Eff[R, A])(implicit m : Member.Aux[Console, R, U]): Eff[U, A] =
    runConsoleToPrinter(m => println(m))(w)

  /**
   * This interpreter prints messages to a printing function
   */
  def runConsoleToPrinter[R <: Effects, U <: Effects, A](printer: String => Unit)(w: Eff[R, A])(implicit m : Member.Aux[Console, R, U]) = {
    val recurse = new StateRecurse[Console, A, A] {
      type S = Unit
      val init = ()

      def apply[X](x: Console[X], s: Unit): (X, Unit) =
        Tag.unwrap(x) match {
          case w => (w.run._2, printer(w.run._1))
        }

      def finalize(a: A, s: Unit): A =
        a
    }

    interpretState1((a: A) => a)(recurse)(w)
  }

}
