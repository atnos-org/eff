package org.atnos.eff

sealed trait Effects

object Effects extends Effects

trait NoEffect extends Effects

trait |:[H[_], T] extends Effects
trait |::[H[_], T[_]] extends Effects

/** this trait shows that a list of effects can be translated to a tree of effects */
trait EffectsToFx[F] { outer =>
  type X

  def toEffects[V](u: Union[X, V]): Union[F, V]
  def toFx[V](u: Union[F, V]): Union[X, V]

  def reverse: FxToEffects[X] = new FxToEffects[X] {
    type X = F

    def toEffects[V](u: Union[outer.X, V]): Union[F, V] = outer.toEffects(u)
    def toFx[V](u: Union[F, V]): Union[outer.X, V] = outer.toFx(u)
  }

}

object EffectsToFx extends LowerEffectsToFx1 {

  type Aux[F, E] = EffectsToFx[F] { type X = E }

  /** one effect in a list can be transformed to a tree of effects */
  implicit def one[H[_]]: EffectsToFx.Aux[H |: NoEffect, Fx1[H]] =
    new EffectsToFx[H |: NoEffect] {
      type X = Fx1[H]

      def toEffects[V](u: Union[Fx1[H], V]): Union[H |: NoEffect, V] =
        u.asInstanceOf[Union[H |: NoEffect, V]]

      def toFx[V](u: Union[H |: NoEffect, V]): Union[Fx1[H], V] =
        u.asInstanceOf[Union[Fx1[H], V]]
    }


}

trait LowerEffectsToFx1 extends LowerEffectsToFx2 {

  /** two effects in a list can be transformed to a tree of effects */
  implicit def two[H1[_], H2[_]]: EffectsToFx.Aux[H1 |: H2 |: NoEffect, Fx2[H1, H2]] =
    new EffectsToFx[H1 |: H2 |: NoEffect] {
      type X = Fx2[H1, H2]

      def toEffects[V](u: Union[Fx2[H1, H2], V]): Union[H1 |: H2 |: NoEffect, V] =
        u.asInstanceOf[Union[H1 |: H2 |: NoEffect, V]]

      def toFx[V](u: Union[H1 |: H2 |: NoEffect, V]): Union[Fx2[H1, H2], V] =
        u.asInstanceOf[Union[Fx2[H1, H2], V]]
    }
}

trait LowerEffectsToFx2 extends LowerEffectsToFx3 {

  /** two effects in a list can be transformed to a tree of effects */
  implicit def twos[H1[_], H2[_]]: EffectsToFx.Aux[H1 |:: H2, Fx2[H1, H2]] =
    new EffectsToFx[H1 |:: H2] {
      type X = Fx2[H1, H2]

      def toEffects[V](u: Union[Fx2[H1, H2], V]): Union[H1 |:: H2, V] =
        u.asInstanceOf[Union[H1 |:: H2, V]]

      def toFx[V](u: Union[H1 |:: H2, V]): Union[Fx2[H1, H2], V] =
        u.asInstanceOf[Union[Fx2[H1, H2], V]]
    }

}

trait LowerEffectsToFx3 extends LowerEffectsToFx4 {
  /** three effects in a list can be transformed to a tree of effects */
  implicit def three[H1[_], H2[_], H3[_]]: EffectsToFx.Aux[H1 |: H2 |: H3 |: NoEffect, Fx3[H1, H2, H3]] =
    new EffectsToFx[H1 |: H2 |: H3 |: NoEffect] {
      type X = Fx3[H1, H2, H3]

      def toEffects[V](u: Union[Fx3[H1, H2, H3], V]): Union[H1 |: H2 |: H3 |: NoEffect, V] =
        u.asInstanceOf[Union[H1 |: H2 |: H3 |: NoEffect, V]]

      def toFx[V](u: Union[H1 |: H2 |: H3 |: NoEffect, V]): Union[Fx3[H1, H2, H3], V] =
        u.asInstanceOf[Union[Fx3[H1, H2, H3], V]]
    }
}

trait LowerEffectsToFx4 {

  /**
    * if T can be transformed to a tree of effects L
    * and H can be prepended to L
    * then H |: T can be transformed to a tree of effects
    */
  implicit def recursiveToEffects[H[_], T, L, R](implicit next: EffectsToFx.Aux[T, L],
                                                 prepend: Prepend.Aux[H, L, R]): EffectsToFx.Aux[H |: T, R] =
    new EffectsToFx[H |: T] {
      type X = R

      def toEffects[V](u: Union[R, V]): Union[H |: T, V] =
        u.asInstanceOf[Union[H |: T, V]]

      def toFx[V](u: Union[H |: T, V]): Union[R, V] =
        u.asInstanceOf[Union[R, V]]
    }

}

/** this trait shows that a tree of effects can be translated to a list of effects */
trait FxToEffects[F] { outer =>
  type X

  def toEffects[V](u: Union[F, V]): Union[X, V]
  def toFx[V](u: Union[X, V]): Union[F, V]

  def reverse: EffectsToFx[X] = new EffectsToFx[X] {
    type X = F

    def toEffects[V](u: Union[F, V]): Union[outer.X, V] = outer.toEffects(u)
    def toFx[V](u: Union[outer.X, V]): Union[F, V] = outer.toFx(u)
  }
}

object FxToEffects extends LowerFxToEffects1 {

  type Aux[F, E] = FxToEffects[F] { type X = E }

  implicit def one[H[_]]: FxToEffects.Aux[Fx1[H], H |: NoEffect] =
    new FxToEffects[Fx1[H]] {
      type X = H |: NoEffect

      def toEffects[V](u: Union[Fx1[H], V]): Union[H |: NoEffect, V] =
        u.asInstanceOf[Union[H |: NoEffect, V]]

      def toFx[V](u: Union[H |: NoEffect, V]): Union[Fx1[H], V] =
        u.asInstanceOf[Union[Fx1[H], V]]
    }


}

trait LowerFxToEffects1 extends LowerFxToEffects2 {

  /** two effects in a list can be transformed to a tree of effects */
  implicit def two[H1[_], H2[_]]: FxToEffects.Aux[Fx2[H1, H2], H1 |: H2 |: NoEffect] =
    new FxToEffects[Fx2[H1, H2]] {
      type X = H1 |: H2 |: NoEffect

      def toEffects[V](u: Union[Fx2[H1, H2], V]): Union[H1 |: H2 |: NoEffect, V] =
        u.asInstanceOf[Union[H1 |: H2 |: NoEffect, V]]

      def toFx[V](u: Union[H1 |: H2 |: NoEffect, V]): Union[Fx2[H1, H2], V] =
        u.asInstanceOf[Union[Fx2[H1, H2], V]]
    }
}

trait LowerFxToEffects2 extends LowerFxToEffects3 {

  /** two effects in a list can be transformed to a tree of effects */
  implicit def twos[H1[_], H2[_]]: FxToEffects.Aux[Fx2[H1, H2], H1 |:: H2] =
    new FxToEffects[Fx2[H1, H2]] {
      type X = H1 |:: H2

      def toEffects[V](u: Union[Fx2[H1, H2], V]): Union[H1 |:: H2, V] =
        u.asInstanceOf[Union[H1 |:: H2, V]]

      def toFx[V](u: Union[H1 |:: H2, V]): Union[Fx2[H1, H2], V] =
        u.asInstanceOf[Union[Fx2[H1, H2], V]]
    }

}

trait LowerFxToEffects3 extends LowerFxToEffects4 {
  /** three effects in a list can be transformed to a tree of effects */
  implicit def three[H1[_], H2[_], H3[_]]: FxToEffects.Aux[Fx3[H1, H2, H3], H1 |: H2 |: H3 |: NoEffect] =
    new FxToEffects[Fx3[H1, H2, H3]] {
      type X = H1 |: H2 |: H3 |: NoEffect

      def toEffects[V](u: Union[Fx3[H1, H2, H3], V]): Union[H1 |: H2 |: H3 |: NoEffect, V] =
        u.asInstanceOf[Union[H1 |: H2 |: H3 |: NoEffect, V]]

      def toFx[V](u: Union[H1 |: H2 |: H3 |: NoEffect, V]): Union[Fx3[H1, H2, H3], V] =
        u.asInstanceOf[Union[Fx3[H1, H2, H3], V]]
    }
}

trait LowerFxToEffects4 {

  /**
    * if T can be transformed to a tree of effects L
    * and H can be prepended to L
    * then H |: T can be transformed to a tree of effects
    */
  implicit def recursiveToEffects[H[_], T, L, R](implicit next: FxToEffects.Aux[T, L],
                                                 prepend: Prepend.Aux[H, R, L]): FxToEffects.Aux[R, H |: T] =
    new FxToEffects[R] {
      type X = H |: T

      def toEffects[V](u: Union[R, V]): Union[H |: T, V] =
        u.asInstanceOf[Union[H |: T, V]]

      def toFx[V](u: Union[H |: T, V]): Union[R, V] =
        u.asInstanceOf[Union[R, V]]
    }

}
