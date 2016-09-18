package org.atnos.eff.syntax

import org.atnos.eff._
import cats._

object optimise extends optimise

trait optimise {

  implicit class OptimiseOps[R, A](e: Eff[R, A]) {

    def optimise[T[_]](optimisable: Optimisable[T])(implicit member: T /= R): Eff[R, A] =
      Optimise.optimise[R, T, A](e)(optimisable)

    def optimiseSemigroup[T[_]](distribute: Any => List[Any])(implicit member: T /= R, semigroup: Semigroup[T[Any]]): Eff[R, A] =
      Optimise.optimiseSemigroup[R, T, A](e)(distribute)

    def optimiseBatching[T[_]](implicit member: T /= R, semigroup: Semigroup[T[Any]]): Eff[R, A] =
      Optimise.optimiseBatching[R, T, A](e)
  }

}
