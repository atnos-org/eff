package org.atnos.eff

import cats._

trait Optimise {

  /**
   * Optimise the applicative calls in a program using a given effect T by replacing them with
   * other "optimised" calls.
   *
   * This operation relies on well-behaved "Optimisable" instances.
   *
   */
  def optimise[R, T[_], A](eff: Eff[R, A])(optimisable: Optimisable[T])(implicit m: T /= R): Eff[R, A] =
    eff match {
      case ImpureAp(unions, map) =>
        val collected = unions.extract
        optimisable.optimise(collected.effects) match {
          case Nil =>
            eff

          case e :: rest =>
            ImpureAp(Unions(m.inject(e), rest.map(m.inject)), ls => map(optimisable.distribute(ls)))
        }

      case _ => eff
    }

  def optimiseSemigroup[R, T[_], A](eff: Eff[R, A])(distribute: Any => List[Any])(implicit m: T /= R, semigroup: Semigroup[T[Any]]): Eff[R, A] =
    optimise[R, T, A](eff)(Optimisable.fromSemigroup(distribute))

  def optimiseBatching[R, T[_], A](eff: Eff[R, A])(implicit m: T /= R, semigroup: Semigroup[T[Any]]): Eff[R, A] =
    optimise[R, T, A](eff)(Optimisable.batching)

}

object Optimise extends Optimise

/**
 * "Optimisable" effects are effects where you can define:
 *
 *  - an optimiser function, "optimise": to transform a list of effects returning some values X, Y, Z... into a list of other effects returning
 *   other values A, B, C
 *
 *  - a mapping, "distribute": to map back the values A, B, C to the expected values X, Y, Z
 *
 *  The expected (informal) law is that, for a given applicative interpreter of T, `run: List[T[Any]] => List[Any]`, we have:
 *
 *  distribute(run(optimise(effects))) ==
 *  run(effects)
 *
 *  NOTE: If an Optimisable instance doesn't satisfy this law, there will be runtime errors!!!
 *
 */
trait Optimisable[T[_]] {
  def optimise(effects: List[T[Any]]): List[T[Any]]
  def distribute(results: List[Any]): List[Any]
}

object Optimisable {

  /**
   * create an Optimisable[T] given a Semigroup for T[_].
   *
   * The Semigroup helps us reduce all the effects to one.
   * That effect returns a value X which you must be able to map back to the list of
   * values which would be created without optimisation
   */
  def fromSemigroup[T[_]](distribute: Any => List[Any])(implicit semigroup: Semigroup[T[Any]]): Optimisable[T] = new Optimisable[T] {
    def optimise(effects: List[T[Any]]): List[T[Any]] =
      effects.reduceOption(semigroup.combine).toList

    def distribute(results: List[Any]): List[Any] =
      results match {
        case Nil => Nil
        case head :: _ => distribute(results)
      }
  }

  /**
   * batching can be used when the Semigroup used to aggregate effects turns them
   * into one effect returning a list of values.
   */
  def batching[T[_]](implicit semigroup: Semigroup[T[Any]]): Optimisable[T] = new Optimisable[T] {
    def optimise(effects: List[T[Any]]): List[T[Any]] =
      effects.reduceOption(semigroup.combine).toList

    def distribute(results: List[Any]): List[Any] =
      results.map {
        case xs: List[Any] => xs
        case other => List(other)
      }.flatten
  }
}

