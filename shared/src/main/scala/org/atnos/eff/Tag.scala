package org.atnos.eff

import cats._

/**
 * Taken from Scalaz
 */
object Tag {
  private[eff] type Tagged[A, T] = {type Tag = T; type Self = A}

  type @@[T, Tag] = Tagged[T, Tag]

  /** `subst` specialized to `Id`.
    *
    * @todo According to Miles, @specialized doesn't help here. Maybe manually specialize.
    */
  @inline def apply[@specialized A, T](a: A): A @@ T = a.asInstanceOf[A @@ T]

  /** `unsubst` specialized to `Id`. */
  @inline def unwrap[@specialized A, T](a: A @@ T): A = unsubst[A, Id, T](a)

  /** Remove the tag `T`, leaving `A`. */
  def unsubst[A, F[_], T](fa: F[A @@ T]): F[A] = fa.asInstanceOf[F[A]]

  /** @see `Tag.of` */
  final class TagOf[T] private[Tag]()
      extends (Id ~> ({type λ[α] = α @@ T})#λ) {
    /** Like `Tag.apply`, but specify only the `T`. */
    def apply[A](a: A): A @@ T = Tag.apply(a)

    /** Like `Tag.unwrap`, but specify only the `T`. */
    def unwrap[A](a: A @@ T): A = Tag.unwrap(a)
  }

  /** Variants of `apply`, `subst`, and `unsubst` that require
    * specifying the tag type but are more likely to infer the other
    * type parameters.
    */
  def of[T]: TagOf[T] = new TagOf[T]
}

