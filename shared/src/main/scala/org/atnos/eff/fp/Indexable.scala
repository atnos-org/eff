package org.atnos.eff.fp

/**
 * Typeclass for sequences which can be indexed
 */
trait Indexable[F[_]] {
  type Key
  def build[A](list: List[(A, Key)]): F[A]
  def list[A](fa: F[A]): List[(A, Key)]
}

object Indexable {
  def apply[F[_]](implicit ev: Indexable[F]): Indexable[F] =
    ev

  implicit class IndexableOps[F[_] : Indexable, A](fa: F[A]) {
    val indexable: Indexable[F] =
      Indexable[F]

    def list: List[(A, indexable.Key)] =
      indexable.list(fa)
  }

  implicit val listIndexable: Indexable[List] = new Indexable[List] {
    type Key = Int

    def list[A](fa: List[A]): List[(A, Key)] =
      fa.zipWithIndex

    def build[A](list: List[(A, Key)]): List[A] =
      list.sortBy(_._2).map(_._1)
  }

  implicit def vectorIndexable: Indexable[Vector] = new Indexable[Vector] {
    type Key = Int

    def list[A](fa: Vector[A]): List[(A, Key)] =
      fa.toList.zipWithIndex

    def build[A](list: List[(A, Key)]): Vector[A] =
      list.sortBy(_._2).map(_._1).toVector
  }

}
