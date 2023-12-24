package org.atnos.eff

/**
 * This trait provides a way to rewrite applicative effects
 * when there is an operation allowing the batching of some effects based on the Batchable typeclass
 */
trait Batch {

  def batch[R, T[_], A](eff: Eff[R, A])(implicit batchable: Batchable[T], m: T /= R): Eff[R, A] =
    eff match {
      case ImpureAp(unions, continuation, last) =>
        // extract only the effects of type M
        val collected = unions.extract

        // we zip each effect with its indice in the original ImpureAp effect list
        // this allows us to recreate a 'map' function for the rewritten ImpureAp
        // where the result of each effect after interpretation will be at the right place as a argument to the
        // 'map' function
        collected.effects zip collected.indices match {
          case v if v.isEmpty => eff

          case e +: rest =>
            // batch any effects which can be batched together
            // by using the Batched datastructure keeping track
            // of both unbatched and batch effects
            val result: Batched[T] = rest.foldLeft(Batched.single(e)) { case (batched, (effect, i)) =>
              batchable.batch[Any, Any](batched.batchedEffect.asInstanceOf[T[Any]], effect) match {
                case Some(b) => batched.fuse(b, i)
                case None => batched.append(effect, i)
              }
            }

            result.effects match {
              case v if v.isEmpty =>
                eff

              case (e1: T[?]) +: rest1 =>
                ImpureAp(
                  Unions(m.inject(e1), rest1.map(r => m.inject(r.asInstanceOf[T[Any]])) ++ collected.otherEffects),
                  // the map operation has to reorder the results based on what could be batched or not
                  continuation.contramap(ls => reorder(ls, result.keys ++ collected.otherIndices)),
                  last
                )
            }
        }

      case _ => eff
    }

  // reorder an input list based on the expected indices for that list
  private def reorder[T[_]](ls: Vector[Any], indices: Vector[Int])(implicit batchable: Batchable[T]): Vector[Any] =
    indices.zip(flatten(ls)).sortBy(_._1).map(_._2)

  // the result of batching
  private def flatten[T[_]](ls: Vector[Any])(implicit batchable: Batchable[T]): Vector[Any] =
    ls match {
      case xs :+ z =>
        xs ++ batchable.distribute(z.asInstanceOf[batchable.Z])

      case v if v.isEmpty =>
        Vector.empty
    }

}

object Batch extends Batch

/**
 * The Batched classes are used to store unbatched and batched effects
 * depending on the result of the Batchable typeclass
 *
 * The assumption is that the order of the effects in 'effects'
 * correspond to the order of the keys in 'keys'
 *
 */
private sealed trait Batched[T[_]] {
  def effects: Vector[T[?]]
  def keys: Vector[Int]
  def batchedEffect: T[?]

  def append(ty: T[?], key: Int): Batched[T]
  def fuse(ty: T[?], key: Int): Batched[T]
}

private object Batched {
  def single[T[_], X](txi: (T[X], Int)): Batched[T] =
    Single(txi._1, Vector(txi._2))
}

private case class Composed[T[_]](unbatched: Vector[Batched[T]], batched: Single[T]) extends Batched[T] {
  def effects: Vector[T[?]] = unbatched.flatMap(_.effects)
  def keys: Vector[Int] = unbatched.flatMap(_.keys) ++ batched.keys
  def batchedEffect: T[?] = batched.batchedEffect

  def append(ty: T[?], key: Int): Batched[T] =
    copy(unbatched = unbatched :+ Batched.single[T, Any]((ty.asInstanceOf[T[Any]], key)))

  def fuse(ty: T[?], key: Int): Batched[T] =
    copy(batched = Single(ty, batched.keys :+ key))
}

private case class Single[T[_]](tx: T[?], keys: Vector[Int]) extends Batched[T] {
  def effects: Vector[T[?]] = Vector(tx)
  def batchedEffect = tx

  def append(ty: T[?], key: Int): Batched[T] =
    Composed(Vector(Batched.single((tx.asInstanceOf[T[Any]], key))), this)

  def fuse(ty: T[?], key: Int): Batched[T] =
    Single(ty, keys :+ key)
}
