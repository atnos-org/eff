package org.atnos.eff

import Eff._
import cats._
import cats.syntax.all._
import scala.annotation.tailrec

trait ChooseInterpretation {

  def runChoose[R, U, A, F[_]: Alternative](r: Eff[R, A])(implicit m: Member.Aux[Choose, R, U]): Eff[U, F[A]] = {
    def lastRun(l: Last[R]): Last[U] =
      l match {
        case Last(None) => Last[U](None)
        case Last(Some(last)) => Last.eff(runChoose[R, U, Unit, F](last.value).as(()))
      }

    val alternativeF = Alternative[F]

    @tailrec
    def go(stack: List[Eff[R, A]], result: Eff[U, F[A]] = EffMonad[U].pure(alternativeF.empty), resultLast: Option[Last[U]] = None): Eff[U, F[A]] =
      stack match {
        case Nil =>
          resultLast match {
            case Some(last) => result.addLast(last)
            case None => result
          }

        case e :: rest =>
          e match {
            case Pure(a, last) =>
              go(rest, (EffMonad[U].pure(alternativeF.pure(a)), result).mapN(alternativeF.combineK), resultLast.map(_ <* lastRun(last)))

            case Impure(NoEffect(a), c, last) =>
              runChoose(c(a).addLast(last))

            case Impure(u: Union[_, _], c, last) =>
              m.project(u) match {
                case Left(u1) =>
                  val r1 = Impure(u1, c.interpret(runChoose[R, U, A, F])(_.interpret(l => runChoose[R, U, Unit, F](l).void))).addLast(lastRun(last))
                  go(rest, (r1, result).mapN(alternativeF.combineK))

                case Right(choose) =>
                  choose match {
                    case ChooseZero() => go(rest, result)
                    case _ =>
                      val continuation = c.asInstanceOf[Continuation[R, Boolean, A]]
                      go(continuation(false) :: continuation(true) :: rest, result)
                  }
              }

            case ap @ ImpureAp(_, _, _) =>
              go(ap.toMonadic :: rest, result)
          }
      }

    go(List(r))
  }

}

object ChooseInterpretation extends ChooseInterpretation
