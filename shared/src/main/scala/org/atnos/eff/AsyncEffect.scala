package org.atnos.eff

import cats.implicits._
import org.atnos.eff.all._

trait AsyncEffect {

  type _async[R] = Async |= R
  type _Async[R] = Async <= R

}

trait AsyncInterpretation {

  def asyncAttempt[R, A](e: Eff[R, A])(implicit async: Async /= R): Eff[R, Throwable Either A] = {
    e match {
      case Pure(a, last) =>
        pure[R, Throwable Either A](Either.right(a)).addLast(last)

      case Impure(u, c, last) =>
        async.extract(u) match {
          case Some(tx) =>
            val union = async.inject(Async.attempt(tx))

            Impure(union, Arrs.singleton { ex: (Throwable Either u.X) =>
              ex match {
                case Right(x) => asyncAttempt(c(x))
                case Left(t) => pure(Either.left(t))
              }
            }, last)

          case None => Impure(u, Arrs.singleton((x: u.X) => asyncAttempt(c(x))), last)
        }

      case ImpureAp(unions, continuation, last) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          async.extract(u) match {
            case Some(tx) => async.inject(Async.attempt(tx))
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val collected = unions.extract(async)
        val continuation1 = Arrs.singleton[R, List[Any], Throwable Either A] { ls: List[Any] =>
          val xors =
            ls.zipWithIndex.collect { case (a, i) =>
              if (collected.indices.contains(i)) a.asInstanceOf[Throwable Either Any]
              else Either.right(a)
            }.sequence

          xors match {
            case Left(t)     => pure(Either.left(t))
            case Right(anys) => asyncAttempt(continuation(anys))
          }
        }

        ImpureAp(materializedUnions, continuation1, last)
    }
  }

  implicit final def toAttemptOps[R, A](e: Eff[R, A]): AttemptOps[R, A] = new AttemptOps[R, A](e)

}

final class AttemptOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def asyncAttempt(implicit async: Async /= R): Eff[R, Throwable Either A] =
    AsyncInterpretation.asyncAttempt(e)
}

object AsyncInterpretation extends AsyncInterpretation


