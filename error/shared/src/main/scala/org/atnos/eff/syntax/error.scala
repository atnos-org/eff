package org.atnos.eff
package syntax

import org.atnos.eff.ErrorEffect.*
import scala.reflect.ClassTag

object error extends error

trait error {
  given errorExtension: AnyRef with {
    extension [R, A](action: Eff[R, A]) {

      def runError(using m: Member[ErrorOrOk, R]): Eff[m.Out, Either[Error, A]] =
        ErrorEffect.runError(action)(using m.aux)

      def andFinally(last: Eff[R, Unit])(using ErrorOrOk <= R): Eff[R, A] =
        ErrorEffect.andFinally(action, last)

      def orElse(action2: Eff[R, A])(using ErrorOrOk <= R): Eff[R, A] =
        ErrorEffect.orElse(action, action2)

      def ignore[E <: Throwable: ClassTag](using ErrorOrOk <= R): Eff[R, Unit] =
        ErrorEffect.ignoreException(action)
    }

    extension [A](c: Either[Error, A]) {
      def toErrorSimpleMessage: Option[String] =
        c match {
          case Left(e) => Some(e.simpleMessage)
          case _ => None
        }

      def toErrorFullMessage: Option[String] =
        c match {
          case Left(e) => Some(e.fullMessage)
          case _ => None
        }
    }

    extension (e: Error) {
      def simpleMessage: String =
        e match {
          case Left(t) => render(t)
          case Right(m) => m
        }

      def fullMessage: String =
        e match {
          case Left(t) => renderWithStack(t)
          case Right(m) => m
        }
    }
  }
}
