package org.atnos.eff

import cats.*

case class Evaluate[F, A](run: Either[Either[Throwable, F], cats.Eval[A]])

object Evaluate {
  def ok[F, A](a: => A) = Evaluate[F, A](Right(cats.Eval.later(a)))
  def eval[F, A](a: Eval[A]) = Evaluate[F, A](Right(a))
  def error[F, A](a: Either[Throwable, F]) = Evaluate[F, A](Left(a))
  def fail[F, A](f: F) = error[F, A](Right(f))
  def exception[F, A](t: Throwable) = error[F, A](Left(t))
}
