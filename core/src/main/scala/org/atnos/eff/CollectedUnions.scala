package org.atnos.eff

/**
 * Collection of effects of a given type from a Unions objects
 *
 */
case class CollectedUnions[M[_], R, U](
  effects: Vector[M[Any]],
  otherEffects: Vector[Union[U, Any]],
  indices: Vector[Int],
  otherIndices: Vector[Int]
) {

  def continuation[A](continueWith: Continuation[R, Vector[Any], A], m: Member.Aux[M, R, U]): Continuation[R, Vector[Any], A] =
    otherEffects match {
      case v if v.isEmpty =>
        continueWith

      case o +: rest =>
        Continuation.lift[R, Vector[Any], A](
          ls => ImpureAp[R, Any, A](Unions(m.accept(o), rest.map(m.accept)), continueWith.contramap(reorder(ls, _))),
          continueWith.onNone
        )
    }

  def continuation[A](continueWith: Continuation[U, Vector[Any], A]): Continuation[U, Vector[Any], A] =
    otherEffects match {
      case v if v.isEmpty =>
        continueWith

      case o +: rest =>
        Continuation.lift[U, Vector[Any], A](ls => ImpureAp[U, Any, A](Unions(o, rest), continueWith.contramap(reorder(ls, _)), continueWith.onNone))
    }

  def othersEff[A](continueWith: Continuation[U, Vector[Any], A]): Eff[U, A] =
    otherEffects match {
      case v if v.isEmpty =>
        continueWith(Vector.empty)

      case o +: rest =>
        ImpureAp[U, Any, A](Unions(o, rest), continueWith)
    }

  private def reorder(ls: Vector[Any], xs: Vector[Any]): Vector[Any] =
    (ls.zip(indices) ++ xs.zip(otherIndices)).sortBy(_._2).map(_._1)

}
