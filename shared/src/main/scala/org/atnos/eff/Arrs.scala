package org.atnos.eff

import cats._

import scala.annotation.tailrec

/**
 * Sequence of monadic functions from A to B: A => Eff[B]
 *
 * Internally it is represented as a Vector of functions:
 *
 *  A => Eff[R, X1]; X1 => Eff[R, X2]; X2 => Eff[R, X3]; ...; X3 => Eff[R, B]
 *
 */
case class Arrs[R, A, B](functions: Vector[Any => Eff[R, Any]]) extends (A => Eff[R, B]) {

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

  /** map the last value */
  def map[C](f: B => C): Arrs[R, A, C] =
    Arrs(functions :+ ((x: Any) => Eff.pure[R, Any](f(x.asInstanceOf[B]).asInstanceOf[Any])))

  /**
   * execute this monadic function
   *
   * This method is stack-safe
   */
  def apply(a: A): Eff[R, B] = {
    @tailrec
    def go(fs: Vector[Any => Eff[R, Any]], v: Any, last: Last[R] = Last.none[R]): Eff[R, B] = {
      fs match {
        case Vector() =>
          Pure[R, B](v.asInstanceOf[B], last)

        case Vector(f) =>
          f(v).asInstanceOf[Eff[R, B]].addLast(last)

        case f +: rest =>
          f(v) match {
            case Pure(a1, l1) =>
              go(rest, a1, last *> l1)

            case Impure(u, q, l) =>
              Impure[R, u.X, B](u, q.copy(q.functions ++ rest), last *> l)

            case ap @ ImpureAp(unions, q, l) =>
              ImpureAp[R, unions.X, B](unions, q.copy(q.functions ++ rest), last *> l)
          }
      }
    }

    go(functions, a)
  }

  def contramap[C](f: C => A): Arrs[R, C, B] =
    Arrs(((c: Any) => Eff.EffMonad[R].pure(f(c.asInstanceOf[C]).asInstanceOf[Any])) +: functions)

  def transform[U, M[_], N[_]](t: ~>[M, N])(implicit m: Member.Aux[M, R, U], n: Member.Aux[N, R, U]): Arrs[R, A, B] =
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


