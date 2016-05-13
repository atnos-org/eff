package org.atnos.eff

import scala.util.control.NonFatal
import cats.data._
import Xor._
import Eff._
import Interpret._

import scala.concurrent._
import duration._

/**
 * Effect for Future computations
 */
trait FutureEffect extends
  FutureCreation with
  FutureInterpretation

object FutureEffect extends FutureEffect

trait FutureCreation {
  def sync[R, A](a: A)(implicit m: Member[Future, R]): Eff[R, A] =
    pure(a)

  def async[R, A](a: =>A)(implicit m: Member[Future, R], ec: ExecutionContext): Eff[R, A] =
    send(Future(a))
}

trait FutureInterpretation {

//  def awaitFuture[R <: Effects, U <: Effects, A](r: Eff[R, A])(atMost: FiniteDuration)
//    (implicit m: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable Xor A] = {
//
//    r match {
//      case Pure(a) => Eff.pure(Xor.Right(a))
//      case Impure(u, c) =>
//        m.project(u) match {
//          case Xor.Left(u1) => Impure(u1, Arrs.singleton((x: u.X) => awaitFuture(c(x))(atMost)))
//          case Xor.Right(fv) =>
//            try { awaitFuture(c(Await.result(fv, atMost)))(atMost) }
//            catch { case NonFatal(t) => Eff.pure(Left(t)) }
//
//        }
//
//      case ImpureAp(u, c) =>
//        m.project(u) match {
//          case Xor.Left(u1) => Impure(u1, Arrs.singleton((x: u.X) => awaitFuture(c(x))(atMost)))
//          case Xor.Right(fv) =>
//            try Await.result(fv.map(v => awaitFuture(c(v))(atMost)), atMost)
//            catch { case NonFatal(t) => Eff.pure(Left(t)) }
//        }
//    }
//  }

  def awaitFuture[R <: Effects, U <: Effects, A](r: Eff[R, A])(atMost: FiniteDuration)
      (implicit m: Member.Aux[Future, R, U], ec: ExecutionContext): Eff[U, Throwable Xor A] = {
    val recurse = new Recurse[Future, U, Throwable Xor A] {
      def apply[X](m: Future[X]) =
        try { Left(Await.result(m, atMost)) }
        catch { case NonFatal(t) => Right(Eff.pure(Left(t))) }
    }

    interpret1((a: A) => Right(a): Throwable Xor A)(recurse)(r)
  }

}

object FutureInterpretation extends FutureInterpretation

