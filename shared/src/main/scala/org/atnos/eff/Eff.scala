package org.atnos.eff

import cats.~>

import scala.annotation.tailrec
import cats._
import cats.data._
import cats.implicits._
import Eff._

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
 *    Vector[Any => Eff[R, Any]]
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
 * @see http://okmij.org/ftp/Haskell/extensible/more.pdf
 *
 */
sealed trait Eff[R, A] {

  def map[B](f: A => B): Eff[R, B] =
    EffApplicative[R].map(this)(f)

  def ap[B](f: Eff[R, A => B]): Eff[R, B] =
    EffApplicative[R].ap(f)(this)

  def *>[B](fb: Eff[R, B]): Eff[R, B] =
    EffApplicative.product(this, fb).map { case (a, b) => b }

  def <*[B](fb: Eff[R, B]): Eff[R, A] =
    EffApplicative.product(this, fb).map { case (a, b) => a }

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B] =
    EffMonad[R].flatMap(this)(f)

  def flatten[B](implicit ev: A =:= Eff[R, B]): Eff[R, B] =
    flatMap(a => a)

  /** add one last action to be executed after any computation chained to this Eff value */
  def addLast(l: =>Eff[R, Unit]): Eff[R, A] =
    // force this computation to finish before adding last
    flatMap(a => pure(a).addLast(Last.eff(l)))

  /** add one last action to be executed after any computation chained to this Eff value */
  private[eff] def addLast(l: Last[R]): Eff[R, A]

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
case class Impure[R, X, A](union: Union[R, X], continuation: Arrs[R, X, A], last: Last[R] = Last.none[R]) extends Eff[R, A] {
  def addLast(l: Last[R]): Eff[R, A] =
    Impure[R, X, A](union, continuation, last <* l)
}

/**
 * ImpureAp is a list of independent effects and a pure function
 * creating a value with all the resulting values once all effects have
 * been interpreted.
 *
 * This essentially models a sequence + map operation but it is important to understand that the list of
 * Union objects can represent different effects and be like: List[Option[Int], Future[String], Option[Int]].
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
case class ImpureAp[R, X, A](unions: Unions[R, X], continuation: Arrs[R, List[Any], A], last: Last[R] = Last.none[R]) extends Eff[R, A] {
  def toMonadic: Eff[R, A] =
    Impure[R, unions.X, A](unions.first, unions.continueWith(continuation), last)

  def addLast(l: Last[R]): Eff[R, A] =
    ImpureAp[R, X, A](unions, continuation, last <* l)
}


object Eff extends EffCreation with
  EffInterpretation with
  EffImplicits

trait EffImplicits {

  /**
   * Monad implementation for the Eff[R, ?] type
   */
  implicit final def EffMonad[R]: Monad[Eff[R, ?]] = new Monad[Eff[R, ?]] {
    def pure[A](a: A): Eff[R, A] =
      Pure(a)

    override def map[A, B](fa: Eff[R, A])(f: A => B): Eff[R, B] =
      fa match {
        case Pure(a, l) =>
          pure(f(a)).addLast(l)

        case Impure(union, continuation, last) =>
          Impure(union, continuation map f, last)

        case ImpureAp(unions, continuations, last) =>
          ImpureAp(unions, continuations map f, last)
      }

    /**
     * When flatMapping the last action must still be executed after the next action
     */
    def flatMap[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      fa match {
        case Pure(a, l) =>
          f(a).addLast(l)

        case Impure(union, continuation, last) =>
          Impure(union, continuation.append(f), last)

        case ImpureAp(unions, continuation, last) =>
          ImpureAp(unions, continuation.append(f), last)
      }

    def tailRecM[A, B](a: A)(f: A => Eff[R, Either[A, B]]): Eff[R, B] =
      flatMap(f(a)) {
        case Right(b)   => pure(b)
        case Left(next) => tailRecM(next)(f)
      }

  }

  def EffApplicative[R]: Applicative[Eff[R, ?]] = new Applicative[Eff[R, ?]] {
    def pure[A](a: A): Eff[R, A] =
      Pure(a)

    override def product[A, B](fa: Eff[R, A], fb: Eff[R, B]): Eff[R, (A, B)] =
      ap(map(fb)(b => (a: A) => (a, b)))(fa)

    def ap[A, B](ff: Eff[R, A => B])(fa: Eff[R, A]): Eff[R, B] =
      fa match {
        case Pure(a, last) =>
          ff match {
            case Pure(f, last1)        => Pure(f(a), last1).addLast(last)
            case Impure(u, c, last1)   => ImpureAp(Unions(u, Nil), Arrs.singleton(ls => c(ls.head).map(_(a))), last1 *> last)
            case ImpureAp(u, c, last1) => ImpureAp(u, Arrs.singleton(xs => c(xs).map(_(a))), last1 *> last)
          }

        case Impure(u, c, last) =>
          ff match {
            case Pure(f, last1)          => ImpureAp(Unions(u, Nil), Arrs.singleton(ls => c(ls.head).map(f)), last1 *> last)
            case Impure(u1, c1, last1)   => ImpureAp(Unions(u, List(u1)),  Arrs.singleton(ls => ap(c1(ls(1)))(c(ls.head))), last1 *> last)
            case ImpureAp(u1, c1, last1) => ImpureAp(Unions(u, u1.unions), Arrs.singleton(ls => ap(c1(ls.drop(1)))(c(ls.head))), last1 *> last)
          }
          
        case ImpureAp(unions, c, last) =>
          ff match {
            case Pure(f, last1)         => ImpureAp(unions, c map f, last1 *> last)
            case Impure(u, c1, last1)   => ImpureAp(Unions(unions.first, unions.rest :+ u), Arrs.singleton(ls => ap(c1(ls.last))(c(ls.dropRight(1)))), last1 *> last)
            case ImpureAp(u, c1, last1) => ImpureAp(u append unions, Arrs.singleton(xs => ap(c1(xs.take(u.size)))(c(xs.drop(u.size)))), last1 *> last)
          }

      }
  }

}

object EffImplicits extends EffImplicits

trait EffCreation {
  /** create an Eff[R, A] value from an effectful value of type T[V] provided that T is one of the effects of R */
  def send[T[_], R, V](tv: T[V])(implicit member: T |= R): Eff[R, V] =
    ImpureAp(Unions(member.inject(tv), Nil), Arrs.singleton(xs => pure[R, V](xs.head.asInstanceOf[V])))

  /** use the internal effect as one of the stack effects */
  def collapse[R, M[_], A](r: Eff[R, M[A]])(implicit m: M |= R): Eff[R, A] =
    EffMonad[R].flatMap(r)(mx => send(mx)(m))

  /** create an Eff value for () */
  def unit[R]: Eff[R, Unit] =
    EffMonad.pure(())

  /** create a pure value */
  def pure[R, A](a: A): Eff[R, A] =
    Pure(a)

  /** create a impure value from an union of effects and a continuation */
  def impure[R, X, A](union: Union[R, X], continuation: Arrs[R, X, A]): Eff[R, A] =
    Impure[R, X, A](union, continuation)

  /** apply a function to an Eff value using the applicative instance */
  def ap[R, A, B](a: Eff[R, A])(f: Eff[R, A => B]): Eff[R, B] =
    EffImplicits.EffApplicative[R].ap(f)(a)

  /** use the applicative instance of Eff to traverse a list of values */
  def traverseA[R, F[_] : Traverse, A, B](fs: F[A])(f: A => Eff[R, B]): Eff[R, F[B]] =
    Traverse[F].traverse(fs)(f)(EffImplicits.EffApplicative[R])

  /** use the applicative instance of Eff to sequence a list of values */
  def sequenceA[R, F[_] : Traverse, A](fs: F[Eff[R, A]]): Eff[R, F[A]] =
    Traverse[F].sequence(fs)(EffImplicits.EffApplicative[R])

  /** use the applicative instance of Eff to traverse a list of values, then flatten it */
  def flatTraverseA[R, F[_], A, B](fs: F[A])(f: A => Eff[R, F[B]])(implicit FT: Traverse[F], FM: FlatMap[F]): Eff[R, F[B]] =
    FT.flatTraverse[Eff[R, ?], A, B](fs)(f)(EffImplicits.EffApplicative[R], FM)

  /** use the applicative instance of Eff to sequence a list of values, then flatten it */
  def flatSequenceA[R, F[_], A](fs: F[Eff[R, F[A]]])(implicit FT: Traverse[F], FM: FlatMap[F]): Eff[R, F[A]] =
    FT.flatSequence[Eff[R, ?], A](fs)(EffImplicits.EffApplicative[R], FM)
}

object EffCreation extends EffCreation

trait EffInterpretation {
  /**
   * base runner for an Eff value having no effects at all
   *
   * This runner can only return the value in Pure because it doesn't
   * known how to interpret the effects in Impure
   */
  def run[A](eff: Eff[NoFx, A]): A =
    eff match {
      case Pure(a, Last(Some(l))) => l.value; a
      case Pure(a, Last(None))    => a
      case other                  => sys.error("impossible: cannot run the effects in "+other)
    }

  /**
   * peel-off the only present effect
   */
  def detach[M[_] : Monad, A](eff: Eff[Fx1[M], A]): M[A] =
    Monad[M].tailRecM[Eff[Fx1[M], A], A](eff) {
      case Pure(a, Last(Some(l))) => Monad[M].pure(Left(l.value.as(a)))
      case Pure(a, Last(None))    => Monad[M].pure(Right(a))

      case Impure(u, continuation, last) =>
        u match {
          case Union1(ta) =>
            last match {
              case Last(Some(l)) => ta.map(x => Left(continuation(x).addLast(last)))
              case Last(None)    => ta.map(x => Left(continuation(x)))
            }
        }

      case ap @ ImpureAp(u, continuation, last) =>
        Monad[M].pure(Left(ap.toMonadic))
    }

  /**
   * peel-off the only present effect, using an Applicative instance where possible
   */
  def detachA[M[_], A](eff: Eff[Fx1[M], A])(implicit monad: Monad[M], applicative: Applicative[M]): M[A] =
    Monad[M].tailRecM[Eff[Fx1[M], A], A](eff) {
      case Pure(a, Last(Some(l))) => monad.pure(Left(l.value.as(a)))
      case Pure(a, Last(None))    => monad.pure(Right(a))

      case Impure(u, continuation, last) =>
        u match {
          case Union1(ta) =>
            last match {
              case Last(Some(l)) => ta.map(x => Left(continuation(x).addLast(last)))
              case Last(None)    => ta.map(x => Left(continuation(x)))
            }
        }

      case ap @ ImpureAp(unions, continuation, last) =>
        val effects = unions.unions.collect { case Union1(mx) => mx }
        val sequenced = applicative.sequence(effects)

        last match {
          case Last(Some(l)) => sequenced.map(x => Left(continuation(x).addLast(last)))
          case Last(None)    => sequenced.map(x => Left(continuation(x)))
        }
    }

  /**
   * get the pure value if there is no effect
   */
  def runPure[R, A](eff: Eff[R, A]): Option[A] =
    eff match {
      case Pure(a, Last(Some(l))) => l.value; Option(a)
      case Pure(a, _)             => Option(a)
      case _                      => None
    }

  /**
   * An Eff[R, A] value can be transformed into an Eff[U, A]
   * value provided that all the effects in R are also in U
   */
  def effInto[R, U, A](e: Eff[R, A])(implicit f: IntoPoly[R, U]): Eff[U, A] =
    f(e)


  /**
   * Memoize an effect using a cache
   *
   * all the consecutive effects M[X] leading to the computation of Eff[R, A]
   * will be cached in the cache and retrieved from there if the Eff[R, A] computation is
   * executed again
   */
  def memoizeEffect[R, M[_], A](e: Eff[R, A], cache: Cache, key: AnyRef)(implicit member: M /= R, cached: SequenceCached[M]): Eff[R, A] =
    cache.get(key.toString).map(Eff.pure[R, A]).getOrElse(memoizeEffectSequence(e, cache, key, 0))

  private def memoizeEffectSequence[R, M[_], A](e: Eff[R, A], cache: Cache, key: AnyRef, sequenceKey: Int)(implicit member: M /= R, cached: SequenceCached[M]): Eff[R, A] = {
    val action: Eff[R, A] =
      e match {
        case Pure(a, last) =>
          Pure(a, last)

        case Impure(u, c, last) =>
          member.extract(u) match {
            case Some(tx) => Impure(member.inject(cached(cache, key, sequenceKey, tx)), Arrs.singleton((x: u.X) => memoizeEffectSequence(c(x), cache, key, sequenceKey + 1)), last)
            case None     => Impure(u, Arrs.singleton((x: u.X) => memoizeEffectSequence(c(x), cache, key, sequenceKey)), last)
          }

        case ImpureAp(unions, continuation, last) =>
          var seqKey = sequenceKey
          def incrementSeqKey = { val s = seqKey; seqKey += 1; s }

          def materialize(u: Union[R, Any]): Union[R, Any] =
            member.extract(u) match {
              case Some(tx) => member.inject(cached(cache, key, incrementSeqKey, tx))
              case None => u
            }

          val materializedUnions =
            Unions(materialize(unions.first), unions.rest.map(materialize))

          val continuation1 = Arrs.singleton[R, List[Any], A]((ls: List[Any]) => memoizeEffectSequence(continuation(ls), cache, key, seqKey))
          ImpureAp(materializedUnions, continuation1, last)
      }

    // once all the values for Eff[R, A] have been computed, we can store the final value
    action.map(a => cache.put(key, a))
  }

}

object EffInterpretation extends EffInterpretation

