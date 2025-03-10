package org.atnos.eff

import cats.Monad
import cats.data.Writer
import cats.~>

/**
 * Effects of type R, returning a value of type A
 *
 * It is implemented as a "Free-er" monad with extensible effects:
 *
 *  - the "pure" case is a pure value of type A
 *
 *  - the "impure" case is:
 *     - a disjoint union of possible effects
 *     - a continuation of type X => Eff[R, A] indicating what to do if the current effect is of type M[X]
 *       this type is represented by the `Arrs` type
 *
 *  - the "impure applicative" case is:
 *     - list of disjoint unions of possible effects
 *     - a function to apply to the values resulting from those effects
 *
 * The monad implementation for this type is really simple:
 *
 *  - `point` is Pure
 *  - `bind` simply appends the binding function to the `Arrs` continuation
 *
 * Important:
 *
 *  The list of continuations is NOT implemented as a type sequence but simply as a
 *  {{{
 *    Vector[Any => Eff[R, Any]]
 *  }}}
 *
 *  This means that various `.asInstanceOf` are present in the implementation and could lead
 *  to burns and severe harm. Use with caution!
 *
 *  Similarly the list of effects in the applicative case is untyped and interpreters for those effects
 *  are supposed to create a list of values to feed the mapping function. If an interpreter doesn't
 *  create a list of values of the right size and with the right types, there will be a runtime exception.
 *
 * The Pure, Impure and ImpureAp cases also incorporate a "last" action returning no value but just used
 * for side-effects (shutting down an execution context for example). This action is meant to be executed at the end
 * of all computations, regardless of the number of flatMaps added on the Eff value.
 *
 * Since this last action will be executed, its value never collected so if it throws an exception it is possible
 * to print it by defining the eff.debuglast system property (-Deff.debuglast=true)
 *
 * @see [[https://okmij.org/ftp/Haskell/extensible/more.pdf]]
 *
 */
sealed abstract class Eff[R, A] {
  import Eff.EffApplicative

  def map[B](f: A => B): Eff[R, B] =
    EffApplicative[R].map(this)(f)

  def ap[B](f: Eff[R, A => B]): Eff[R, B] =
    EffApplicative[R].ap(f)(this)

  def product[B](fb: Eff[R, B]): Eff[R, (A, B)] =
    EffApplicative[R].product(this, fb)

  def map2[B, C](fb: Eff[R, B])(f: (A, B) => C): Eff[R, C] =
    EffApplicative[R].map2(this, fb)(f)

  def map2Flatten[B, C](fb: Eff[R, B])(f: (A, B) => Eff[R, C]): Eff[R, C] =
    Monad[Eff[R, *]].flatMap(EffApplicative[R].product(this, fb)) { (a, b) => f(a, b) }

  def *>[B](fb: Eff[R, B]): Eff[R, B] =
    EffApplicative[R].map2(this, fb) { case (_, b) => b }

  def <*[B](fb: Eff[R, B]): Eff[R, A] =
    EffApplicative[R].map2(this, fb) { case (a, _) => a }

  def >>=[B](f: A => Eff[R, B]): Eff[R, B] =
    flatMap(f)

  def >>[B](fb: Eff[R, B]): Eff[R, B] =
    flatMap(_ => fb)

  def <<[B](fb: Eff[R, B]): Eff[R, A] =
    flatMap(a => fb.map(_ => a))

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B] =
    Monad[Eff[R, *]].flatMap(this)(f)

  def flatten[B](using ev: A <:< Eff[R, B]): Eff[R, B] =
    flatMap(ev)

  /** add one last action to be executed after any computation chained to this Eff value */
  def addLast(l: => Eff[R, Unit]): Eff[R, A] =
    addLast(Last.eff(l))

  /** add one last action to be executed after any computation chained to this Eff value */
  def addLast(l: Last[R]): Eff[R, A]

  def into[U](using f: IntoPoly[R, U]): Eff[U, A] =
    Eff.effInto(this)(using f)

  def transform[BR, U, M[_], N[_]](t: ~>[M, N])(using m: Member.Aux[M, R, U], n: Member.Aux[N, BR, U]): Eff[BR, A] =
    Interpret.transform(this, t)(using m, n, IntoPoly.intoSelf[U])

  def translate[M[_], U](t: Translate[M, U])(using m: Member.Aux[M, R, U]): Eff[U, A] =
    Interpret.translate(this)(t)(using m)

  def translateInto[T[_], U](t: Translate[T, U])(using m: MemberInOut[T, R], into: IntoPoly[R, U]): Eff[U, A] =
    interpret.translateInto(this)(t)(using m, into)

  def write[T[_], O](w: Write[T, O])(using MemberInOut[T, R], MemberInOut[Writer[O, *], R]): Eff[R, A] =
    interpret.write(this)(w)

  def augment[T[_], O[_]](w: Augment[T, O])(using MemberInOut[T, R], MemberIn[O, R]): Eff[R, A] =
    interpret.augment(this)(w)

  def runPure: Option[A] =
    Eff.runPure(this)
}

case class Pure[R, A](value: A, last: Last[R] = Last.none[R]) extends Eff[R, A] {
  def addLast(l: Last[R]): Eff[R, A] =
    Pure(value, last <* l)
}

/**
 * Impure is an effect (encoded as one possibility among other effects, a Union)
 * and a continuation providing the next Eff value.
 *
 * This essentially models a flatMap operation with the current effect
 * and the monadic function to apply to a value once the effect is interpreted
 *
 * One effect can always be executed last, just for side-effects
 */
case class Impure[R, X, A](union: Effect[R, X], continuation: Continuation[R, X, A], last: Last[R] = Last.none[R]) extends Eff[R, A] {
  def addLast(l: Last[R]): Eff[R, A] =
    Impure[R, X, A](union, continuation, last <* l)
}

/**
 * ImpureAp is a list of independent effects and a pure function
 * creating a value with all the resulting values once all effects have
 * been interpreted.
 *
 * This essentially models a sequence + map operation but it is important to understand that the list of
 * Union objects can represent different effects and be like: `Vector[Option[Int], Future[String], Option[Int]]`.
 *
 * Interpreting such an Eff value for a given effect (say Option) consists in:
 *
 *  - grouping all the Option values,
 *  - sequencing them
 *  - pass them to a continuation which will apply the 'map' functions when the other effects (Future in the example
 *  above) will have been interpreted
 *
 * VERY IMPORTANT:
 *
 *  - this object is highly unsafe
 *  - the size of the list argument to 'map' must always be equal to the number of unions in the Unions object
 *  - the types of the elements in the list argument to 'map' must be the exact types of each effect in unions.unions
 *
 */
case class ImpureAp[R, X, A](unions: Unions[R, X], continuation: Continuation[R, Vector[Any], A], last: Last[R] = Last.none[R]) extends Eff[R, A] {
  def toMonadic: Eff[R, A] =
    Impure[R, unions.X, A](unions.first, unions.continueWith(continuation), last)

  def addLast(l: Last[R]): Eff[R, A] =
    ImpureAp[R, X, A](unions, continuation, last <* l)
}

object Eff extends EffCreation with EffInterpretation with EffImplicits
