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

  def plusLast(l: =>Eff[R, Unit]): Eff[R, A] =
    addLast(Last.eff(l))

  def addLast(l: Last[R]): Eff[R, A]

}

case class Last[R](value: Option[() => Eff[R, Unit]]) {
  def run: Eff[R, Unit] =
    value.map(_()).getOrElse(pure[R, Unit](()))

  def <*(last: Last[R]): Last[R] =
    (value, last.value) match {
      case (None, None)       => this
      case (Some(r), None)    => this
      case (None, Some(l))    => last
      case (Some(r), Some(l)) => Last(Option(() => r() <* l()))
    }

  def *>(last: Last[R]): Last[R] =
    (value, last.value) match {
      case (None, None)       => this
      case (Some(r), None)    => this
      case (None, Some(l))    => last
      case (Some(r), Some(l)) => Last(Option(() => r() *> l()))
    }
}

object Last {
  def none[R]: Last[R] =
    Last(None)
  
  def eff[R](e: =>Eff[R, Unit]): Last[R] =
    Last(Option(() => e))
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

/**
 * A non-empty list of Unions.
 *
 * It is only partially typed, we just keep track of the type of the first object
 */
case class Unions[R, A](first: Union[R, A], rest: List[Union[R, Any]]) {
  type X = A

  def size: Int =
    rest.size + 1

  def unions: List[Union[R, Any]]=
    first.asInstanceOf[Union[R, Any]] +: rest

  def append[B](others: Unions[R, B]): Unions[R, A] =
    Unions(first, rest ++ others.unions)

  /**
   * create a continuation which will apply the 'map' function
   * if the first effect of this Unions object is interpreted
   */
  def continueWith[B](continuation: Arrs[R, List[Any], B]): Arrs[R, A, B] =
    Arrs.singleton { (x: X) =>
      rest match {
        case Nil    => continuation(x :: Nil)
        case h :: t => ImpureAp[R, h.X, B](Unions[R, h.X](h, t), Arrs.singleton((ys: List[Any]) => continuation(x :: ys)))
      }
    }

  def into[S](f: UnionInto[R, S]): Unions[S, A] =
    Unions[S, A](f(first), rest.map(f.apply))

  /**
   * collect all the M effects and create a continuation for other effects
   * in a stack containing no more M effects
   */
  def project[M[_], U](implicit m: Member.Aux[M, R, U]): CollectedUnions[M, R, U] =
    collect[M, U](m.project)

  /**
   * collect all the M effects and create a continuation for other effects
   * in the same stack
   */
  def extract[M[_]](implicit m: M /= R): CollectedUnions[M, R, R] =
    collect[M, R](u => m.extract(u) match {
      case Some(mx) => Right(mx)
      case None     => Left(u)
    })

  private def collect[M[_], U](collect: Union[R, Any] => Union[U, Any] Either M[Any]): CollectedUnions[M, R, U] = {
    val (effectsAndIndices, othersAndIndices) =
      unions.zipWithIndex.foldLeft((Vector[(M[Any], Int)](), Vector[(Union[U, Any], Int)]())) {
        case ((es, os), (u, i)) =>
          collect(u) match {
            case Right(mx) => (es :+ ((mx, i)), os)
            case Left(o) => (es, os :+ ((o, i)))
          }
      }

    val (effects, indices) = effectsAndIndices.toList.unzip
    val (otherEffects, otherIndices) = othersAndIndices.toList.unzip

    CollectedUnions[M, R, U](effects, otherEffects, indices, otherIndices)
  }

  def transform[M[_]](nat: M ~> M)(implicit m: M /= R): Unions[R, A] =
    Unions(m.transformUnion(nat)(first), rest.map(m.transformUnion(nat)))

}

object Unions {
  def send[M[_], R, X](mx: M[X])(implicit m: MemberIn[M, R]) =
    Unions[R, X](m.inject(mx), Nil)
}

/**
 * Collection of effects of a given type from a Unions objects
 *
 */
case class CollectedUnions[M[_], R, U](effects: List[M[Any]], otherEffects: List[Union[U, Any]], indices: List[Int], otherIndices: List[Int]) {
  def continuation[A](continueWith: List[Any] => Eff[R, A], m: Member.Aux[M, R, U]): Arrs[R, List[Any], A] =
    otherEffects match {
      case Nil       => Arrs.singleton[R, List[Any], A](ls => continueWith(ls))
      case o :: rest => Arrs.singleton[R, List[Any], A](ls => ImpureAp[R, Any, A](Unions(m.accept(o), rest.map(m.accept)), Arrs.singleton(xs => continueWith(reorder(ls, xs)))))
    }

  def continuation[A](continueWith: Arrs[U, List[Any], A]): Arrs[U, List[Any], A] =
    otherEffects match {
      case Nil       => Arrs.singleton[U, List[Any], A](ls => continueWith(ls))
      case o :: rest => Arrs.singleton[U, List[Any], A](ls => ImpureAp[U, Any, A](Unions(o, rest), Arrs.singleton(xs => continueWith(reorder(ls, xs)))))
    }

  def othersEff[A](continueWith: Arrs[U, List[Any], A]): Eff[U, A] =
    otherEffects match {
      case Nil       => continueWith(Nil)
      case o :: rest => ImpureAp[U, Any, A](Unions(o, rest), Arrs.singleton(ls => continueWith(ls)))
    }

  private def reorder(ls: List[Any], xs: List[Any]): List[Any] =
    (ls.zip(indices) ++ xs.zip(otherIndices)).sortBy(_._2).map(_._1)

}


trait UnionInto[R, S] {
  def apply[A](union: Union[R, A]): Union[S, A]
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

    override def map[A, B](fa: Eff[R, A])(f: A => B): Eff[R, B] =
      fa match {
        case Pure(a, l) =>
          pure(f(a)).addLast(l)

        case Impure(union, continuation, last) =>
          fa.flatMap(a => pure(f(a)))

        case ImpureAp(u, c, last) =>
          ImpureAp(u, c map f, last)
      }

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
  def run[A](eff: Eff[NoFx, A]): A =
    eff match {
      case Pure(a, Last(Some(l))) => l(); a
      case Pure(a, Last(None))    => a
      case other                  => sys.error("impossible: cannot run the effects in "+other)
    }

  /**
   * peel-off the only present effect
   */
  def detach[M[_] : Monad, A](eff: Eff[Fx1[M], A]): M[A] = {
    def go(e: Eff[Fx1[M], A]): M[A] = {
      e match {
        case Pure(a, Last(Some(l))) => Monad[M].pure(a) <* detach(l())
        case Pure(a, Last(None))    => Monad[M].pure(a)

        case Impure(u, continuation, last) =>
          u match {
            case Union1(ta) => Monad[M].flatMap(ta)(x => go(continuation(x))) <* detach(last.run)
          }

        case ap @ ImpureAp(u, continuation, last) =>
          detach(ap.toMonadic)
      }
    }
    go(eff)
  }

  /**
   * peel-off the only present effect, using an Applicative instance where possible
   */
  def detachA[M[_], A](eff: Eff[Fx1[M], A])(implicit monad: Monad[M], applicative: Applicative[M]): M[A] = {
    def go(e: Eff[Fx1[M], A]): M[A] = {
      e match {
        case Pure(a, Last(Some(l))) => go(l().as(a))
        case Pure(a, Last(None))    => monad.pure(a)

        case Impure(u, continuation, last) =>
          u match {
            case Union1(ta) => monad.flatMap(ta)(x => go(continuation(x))) <* detachA(last.run)
          }

        case ap @ ImpureAp(unions, continuation, last) =>
          applicative.sequence(unions.unions.collect { case Union1(mx) => mx }).flatMap(x => go(continuation(x))) <* detachA(last.run)
      }
    }
    go(eff)
  }

  /**
   * get the pure value if there is no effect
   */
  def runPure[R, A](eff: Eff[R, A]): Option[A] =
    eff match {
      case Pure(a, Last(Some(l))) => l(); Option(a)
      case Pure(a, _)             => Option(a)
      case _                      => None
    }

  /**
   * An Eff[R, A] value can be transformed into an Eff[U, A]
   * value provided that all the effects in R are also in U
   */
  def effInto[R, U, A](e: Eff[R, A])(implicit f: IntoPoly[R, U]): Eff[U, A] =
    f(e)
}

object EffInterpretation extends EffInterpretation

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
    Arrs(functions :+ ((x: Any) => pure[R, Any](f(x.asInstanceOf[B]).asInstanceOf[Any])))

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

