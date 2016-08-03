package org.atnos.eff

import cats.data._
import Effects._
import cats.data._, Xor._


sealed trait Union[+R, A] {
  type X = A
}

case class Union1[T[_], A](ta: T[A]) extends Union[Fx1[T], A]

sealed trait Union2[R, A] extends Union[R, A]
case class Union2L[L[_], R[_], A](t: L[A]) extends Union2[Fx2[L, R], A]
case class Union2R[L[_], R[_], A](t: R[A]) extends Union2[Fx2[L, R], A]

sealed trait Union3[R, A] extends Union[R, A]
case class Union3L[L[_], M[_], R[_], A](t: L[A]) extends Union3[Fx3[L, M, R], A]
case class Union3M[L[_], M[_], R[_], A](t: M[A]) extends Union3[Fx3[L, M, R], A]
case class Union3R[L[_], M[_], R[_], A](t: R[A]) extends Union3[Fx3[L, M, R], A]

sealed trait UnionAppend[R, A] extends Union[R, A]
case class UnionAppendL[L, R, A](t: Union[L, A]) extends UnionAppend[FxAppend[L, R], A]
case class UnionAppendR[L, R, A](t: Union[R, A]) extends UnionAppend[FxAppend[L, R], A]

