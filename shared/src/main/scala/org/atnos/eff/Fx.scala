package org.atnos.eff

/** one effect, basically a type constructor */
sealed trait Effect[F[_]]

sealed trait Fx

object Fx {
  type prepend[T[_], R] = FxAppend[Fx1[T], R]
  type append[L, R] = FxAppend[L, R]

  type fx1[T1[_]] = Fx1[T1]
  type fx2[T1[_], T2[_]] = Fx2[T1, T2]
  type fx3[T1[_], T2[_], T3[_]] = Fx3[T1, T2, T3]
  type fx4[T1[_], T2[_], T3[_], T4[_]] = FxAppend[Fx1[T1], Fx3[T2, T3, T4]]
  type fx5[T1[_], T2[_], T3[_], T4[_], T5[_]] = FxAppend[Fx2[T1, T2], Fx3[T3, T4, T5]]
  type fx6[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_]] = FxAppend[Fx3[T1, T2, T3], Fx3[T4, T5, T6]]
  type fx7[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], T7[_]] = FxAppend[Fx1[T1], FxAppend[Fx3[T2, T3, T4], Fx3[T5, T6, T7]]]
  type fx8[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], T7[_], T8[_]] = FxAppend[Fx2[T1, T2], FxAppend[Fx3[T3, T4, T5], Fx3[T6, T7, T8]]]
  type fx9[T1[_], T2[_], T3[_], T4[_], T5[_], T6[_], T7[_], T8[_], T9[_]] = FxAppend[Fx3[T1, T2, T3], FxAppend[Fx3[T4, T5, T6], Fx3[T7, T8, T9]]]
}

/**
  * Append an effect at the beginning of a list of effects
  */
final case class FxAppend[L, R](left: L, right: R) extends Fx

final case class Fx1[F[_]](e: Effect[F]) extends Fx
final case class Fx2[L[_], R[_]](left: Effect[L], right: Effect[R]) extends Fx
final case class Fx3[L[_], M[_], R[_]](left: Effect[L], middle: Effect[M], right: Effect[R]) extends Fx

/**
  * Nil case for the list of effects
  */
class NoFx extends Fx

object NoFx extends NoFx
