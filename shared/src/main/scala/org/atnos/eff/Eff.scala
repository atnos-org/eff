package org.atnos.eff

import cats.arrow.NaturalTransformation

import scala.annotation.tailrec
import cats._
import cats.data._, Xor._
import Union._
import Effects._
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
 * @see http://okmij.org/ftp/Haskell/extensible/more.pdf
 *
 */
sealed trait Eff[R, A] {
  def map[B](f: A => B): Eff[R, B] =
    EffMonad[R].map(this)(f)

  def ap[B](f: Eff[R, A => B]): Eff[R, B] =
    EffApplicative[R].ap(f)(this)

  def flatMap[B](f: A => Eff[R, B]): Eff[R, B] =
    EffMonad[R].flatMap(this)(f)

  def flatten[B](implicit ev: A =:= Eff[R, B]): Eff[R, B] =
    flatMap(a => a)
}

case class Pure[R, A](value: A) extends Eff[R, A]

/**
 * union is a disjoint union of effects returning a value of type X (not specified)
 */
case class Impure[R, X, A](union: Union[R, X], continuation: Arrs[R, X, A]) extends Eff[R, A]

/**
 * union is a disjoint union of effects returning a value of type X (not specified)
 */
case class ImpureAp[R, X, A](union: Union[R, X], continuation: Apps[R, X, A]) extends Eff[R, A] {
  def toMonadic: Eff[R, A] =
    Impure[R, union.X, A](union, Arrs.singleton((x: union.X) => continuation(x)))
}

object Eff extends EffCreation with
  EffInterpretation with
  EffImplicits

trait EffImplicits {

  /**
   * Monad implementation for the Eff[R, ?] type
   */
  implicit def EffMonad[R]: Monad[Eff[R, ?]] = new Monad[Eff[R, ?]] {
    def pure[A](a: A): Eff[R, A] =
      Pure(a)

    def flatMap[A, B](fa: Eff[R, A])(f: A => Eff[R, B]): Eff[R, B] =
      fa match {
        case Pure(a) =>
          f(a)

        case Impure(union, continuation) =>
          Impure(union, continuation.append(f))

        case ap @ ImpureAp(_, _) =>
          ap.toMonadic.flatMap(f)
      }
  }

  def EffApplicative[R]: Applicative[Eff[R, ?]] = new Applicative[Eff[R, ?]] {
    def pure[A](a: A): Eff[R, A] =
      Pure(a)

    def ap[A, B](ff: Eff[R, A => B])(fa: Eff[R, A]): Eff[R, B] =
      fa match {
        case Pure(a) =>
          ff.map(f => f(a))

        case Impure(union, continuation) =>
          fa.flatMap(a => ff.map(f => f(a)))

        case ImpureAp(union, continuation) =>
          ImpureAp(union, continuation.append(ff))

      }
  }

}

object EffImplicits extends EffImplicits

trait EffCreation {
  /** create an Eff[R, A] value from an effectful value of type T[V] provided that T is one of the effects of R */
  def send[T[_], R, V](tv: T[V])(implicit member: Member[T, R]): Eff[R, V] =
    impure(member.inject(tv), Arrs.unit)

  /** use the internal effect as one of the stack effects */
  def collapse[R, M[_], A](r: Eff[R, M[A]])(implicit m: Member[M, R]): Eff[R, A] =
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

  /** use the applicative instance of Eff to sequenc a list of values */
  def sequenceA[R, F[_] : Traverse, A](fs: F[Eff[R, A]]): Eff[R, F[A]] =
    Traverse[F].sequence(fs)(EffImplicits.EffApplicative[R])
}

object EffCreation extends EffCreation

trait EffInterpretation {
  /**
   * base runner for an Eff value having no effects at all
   *
   * This runner can only return the value in Pure because it doesn't
   * known how to interpret the effects in Impure
   */
  def run[A](eff: Eff[NoEffect, A]): A =
    eff match {
      case Pure(a) => a
      case other   => sys.error("impossible: cannot run the effects in "+other)
    }

  /**
   * peel-off the only present effect
   */
  def detach[M[_] : Monad, A](eff: Eff[M |: NoEffect, A]): M[A] = {
    def go(e: Eff[M |: NoEffect, A]): M[A] = {
      e match {
        case Pure(a) => Monad[M].pure(a)

        case Impure(UnionNow(mx), continuation) =>
          Monad[M].flatMap(mx)(x => go(continuation(x)))

        case _ =>
          sys.error("impossible")
      }
    }
    go(eff)
  }

  /**
   * get the pure value if there is no effect
   */
  def runPure[R, A](eff: Eff[R, A]): Option[A] =
    eff match {
      case Pure(a) => Option(a)
      case _ => None
    }

  /**
   * An Eff[R, A] value can be transformed into an Eff[U, A]
   * value provided that all the effects in R are also in U
   */
  def effInto[R, U, A](e: Eff[R, A])(implicit f: IntoPoly[R, U, A]): Eff[U, A] =
    f(e)
}

object EffInterpretation extends EffInterpretation

/**
 * Trait for polymorphic recursion into Eff[?, A]
 *
 * The idea is to deal with one effect at the time:
 *
 *  - if the effect stack is M |: R and if U contains M
 *    we transform each "Union[R, X]" in the Impure case into a Union for U
 *    and we try to recurse on other effects present in R
 *
 *  - if the effect stack is M |: NoEffect and if U contains M we
 *    just "inject" the M[X] effect into Eff[U, A] using the Member typeclass
 *    if M is not present when we decompose we throw an exception. This case
 *    should never happen because if there is no other effect in the stack
 *    there should be at least something producing a value of type A
 *
 */
trait IntoPoly[R, U, A] {
  def apply(e: Eff[R, A]): Eff[U, A]
}

object IntoPoly extends IntoPolyLower {

  implicit def intoNoEff[M[_], U, A](implicit m: Member[M, M |: NoEffect], mu: Member[M, U]): IntoPoly[M |: NoEffect, U, A] =
    new IntoPoly[M |: NoEffect, U, A] {
      def apply(e: Eff[M |: NoEffect, A]): Eff[U, A] = {

        e match {
          case Pure(a) =>
            EffMonad[U].pure(a)

          case Impure(u, c) =>
            decompose(u) match {
              case Right(mx) => impure[U, u.X, A](mu.inject(mx), Arrs.singleton(x => effInto(c(x))))
              case Left(u1) => sys.error("impossible")
            }

          case ap @ ImpureAp(u,c) =>
            decompose(u) match {
              case Right(mx) =>
                ImpureAp[U, u.X, A](mu.inject(mx), Apps[U, u.X, A](c.functions.map(f => effInto[M |: NoEffect, U, Any => Any](f))))
              case Left(u1) => sys.error("impossible")
            }
        }
      }
    }

  implicit def intoOne[M[_], R, A]: IntoPoly[R, M |: R, A] =
    new IntoPoly[R, M |: R, A] {
      def apply(e: Eff[R, A]): Eff[M |: R, A] = {
        e match {
          case Pure(a) =>
            EffMonad[M |: R].pure(a)

          case Impure(u, c) =>
            Impure[M |: R, u.X, A](UnionNext(u), Arrs.singleton(x => effInto[R, M |: R, A](c(x))))

          case ap @ ImpureAp(u, c) =>
            ImpureAp[M |: R, u.X, A](UnionNext(u),
              Apps[M |: R, u.X, A](c.functions.map(f => effInto[R, M |: R, Any => Any](f))))
        }
      }
    }
}
trait IntoPolyLower {
  implicit def intoEff[M[_], R, U, A](implicit m: Member[M, M |: R], mu: Member[M, U], recurse: IntoPoly[R, U, A]): IntoPoly[M |: R, U, A] =
    new IntoPoly[M |: R, U, A] {
      def apply(e: Eff[M |: R, A]): Eff[U, A] = {

        e match {
          case Pure(a) =>
            EffMonad[U].pure(a)

          case Impure(u, c) =>
            decompose(u) match {
              case Right(mx) => impure[U, u.X, A](mu.inject(mx), Arrs.singleton(x => effInto(c(x))))
              case Left(u1) => recurse(impure[R, u1.X, A](u1, c.asInstanceOf[Arrs[R, u1.X, A]]))
            }

          case ap @ ImpureAp(u, c) =>
            decompose(u) match {
              case Right(mx) =>
                ImpureAp[U, u.X, A](mu.inject(mx),
                  Apps[U, u.X, A](c.functions.map(f => effInto[M |: R, U, Any => Any](f)(intoEff(m, mu, recurse.asInstanceOf[IntoPoly[R, U, Any => Any]])))))

              case Left(u1) =>
                recurse(ImpureAp[R, u1.X, A](u1, c.asInstanceOf[Apps[R, u1.X, A]]))
            }
        }
      }
    }
}

/**
 * Sequence of monadic functions from A to B: A => Eff[B]
 *
 * Internally it is represented as a Vector of functions:
 *
 *  A => Eff[R, X1]; X1 => Eff[R, X2]; X2 => Eff[R, X3]; ...; X3 => Eff[R, B]
 *
 */
case class Arrs[R, A, B](functions: Vector[Any => Eff[R, Any]]) {

  /**
   * append a new monadic function to this list of functions such that
   *
   * Arrs[R, A, B] => (B => Eff[R, C]) => Arrs[R, A, C]
   *
   */
  def append[C](f: B => Eff[R, C]): Arrs[R, A, C] =
    Arrs(functions :+ f.asInstanceOf[Any => Eff[R, Any]])

  /** map the last returned effect */
  def mapLast(f: Eff[R, B] => Eff[R, B]): Arrs[R, A, B] =
    functions match {
      case Vector() => this
      case fs :+ last => Arrs(fs :+ ((x: Any) => f(last(x).asInstanceOf[Eff[R, B]]).asInstanceOf[Eff[R, Any]]))
    }

  /**
   * execute this monadic function
   *
   * This method is stack-safe
   */
  def apply(a: A): Eff[R, B] = {
    @tailrec
    def go(fs: Vector[Any => Eff[R, Any]], v: Any): Eff[R, B] = {
      fs match {
        case Vector() =>
          Eff.EffMonad[R].pure(v).asInstanceOf[Eff[R, B]]

        case Vector(f) =>
          f(v).asInstanceOf[Eff[R, B]]

        case f +: rest =>
          f(v) match {
            case Pure(a1) =>
              go(rest, a1)

            case Impure(u, q) =>
              Impure[R, u.X, B](u, q.copy(functions = q.functions ++ rest))

            case ap @ ImpureAp(u, continuation) =>
              val monadicArrs = Arrs.singleton((x: u.X) => continuation(x))
              Impure[R, u.X, B](u, monadicArrs.copy(functions = monadicArrs.functions ++ rest))
          }
      }
    }

    go(functions, a)
  }

  def contramap[C](f: C => A): Arrs[R, C, B] =
    Arrs(((c: Any) => Eff.EffMonad[R].pure(f(c.asInstanceOf[C]).asInstanceOf[Any])) +: functions)

  def transform[U, M[_], N[_]](t: NaturalTransformation[M, N])(implicit m: Member.Aux[M, R, U], n: Member.Aux[N, R, U]): Arrs[R, A, B] =
    Arrs(functions.map(f => (x: Any) => Interpret.transform(f(x), t)(m, n)))
}

object Arrs {

  /** create an Arrs function from a single monadic function */
  def singleton[R, A, B](f: A => Eff[R, B]): Arrs[R, A, B] =
    Arrs(Vector(f.asInstanceOf[Any => Eff[R, Any]]))

  /** create an Arrs function with no effect, which is similar to using an identity a => EffMonad[R].pure(a) */
  def unit[R, A]: Arrs[R, A, A] =
    Arrs(Vector())
}

/**
 * Sequence of applicative functions from A to B: Eff[R, A => B]
 *
 */
case class Apps[R, A, B](functions: Vector[Eff[R, Any => Any]]) {

  def append[C](f: Eff[R, B => C]): Apps[R, A, C] =
    Apps(functions :+ f.asInstanceOf[Eff[R, Any => Any]])

  /**
   * execute this data structure as function
   *
   * This method is stack-safe
   */
  def apply(a: A): Eff[R, B] = {
    @tailrec
    def go(fs: Vector[Eff[R, Any => Any]], v: Eff[R, Any]): Eff[R, B] = {
      fs match {
        case Vector() =>
          v.asInstanceOf[Eff[R, B]]

        case Vector(ff) =>
          ff.flatMap(f => v.map(f)).asInstanceOf[Eff[R, B]]

        case ff +: rest =>
          go(rest, ff.flatMap(f => v.map(f)))
      }
    }

    go(functions, Eff.EffMonad[R].pure(a).asInstanceOf[Eff[R, Any]])
  }
}

object Apps {

  /** create an Apps function from a single applicative function */
  def singleton[R, A, B](f: Eff[R, A => B]): Apps[R, A, B] =
    Apps(Vector(f.asInstanceOf[Eff[R, Any => Any]]))

  /** create an Apps function with no effect */
  def unit[R, A]: Apps[R, A, A] =
    Apps(Vector())
}
