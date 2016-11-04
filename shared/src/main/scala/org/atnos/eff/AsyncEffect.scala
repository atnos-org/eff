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
      case Pure(a) => pure[R, Throwable Either A](Either.right(a))

      case Impure(u, c) =>
        async.extract(u) match {
          case Some(tx) =>
            val union = async.inject(tx.attempt)

            Impure(union, Arrs.singleton { ex: (Throwable Either u.X) =>
              ex match {
                case Right(x) => asyncAttempt(c(x))
                case Left(t) => pure(Either.left(t))
              }
            })

          case None => Impure(u, Arrs.singleton((x: u.X) => asyncAttempt(c(x))))
        }

      case ImpureAp(unions, c) =>
        def materialize(u: Union[R, Any]): Union[R, Any] =
          async.extract(u) match {
            case Some(tx) => async.inject(tx.attempt)
            case None => u
          }

        val materializedUnions =
          Unions(materialize(unions.first), unions.rest.map(materialize))

        val collected = unions.extract(async)
        val continuation = Arrs.singleton[R, List[Any], Throwable Either A] { ls: List[Any] =>
          val xors =
            ls.zipWithIndex.collect { case (a, i) =>
              if (collected.indices.contains(i)) a.asInstanceOf[Throwable Either Any]
              else Either.right(a)
            }.sequence

          xors match {
            case Left(t)     => pure(Either.left(t))
            case Right(anys) => asyncAttempt(c(anys))
          }
        }

        ImpureAp(materializedUnions, continuation)
    }
  }

  implicit class AttemptOps[R, A](e: Eff[R, A])(implicit async: Async /= R){
    def asyncAttempt: Eff[R, Throwable Either A] =
      AsyncInterpretation.asyncAttempt(e)
  }
}

object AsyncInterpretation extends AsyncInterpretation

trait Async[+A] {
  def attempt: Async[Throwable Either A]
}
