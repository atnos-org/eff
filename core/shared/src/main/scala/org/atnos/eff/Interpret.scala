package org.atnos.eff

import cats._
import cats.data._
import cats.syntax.all._
import Eff._

/**
 * The Interpret trait provides method to interpret (or "handle") effects.
 *
 * An interpreter generally handles a given effect M and a value Eff[R, A]
 * where M is a member of R.
 *
 * The most general way of interpreting an effect is to implement the Interpreter trait for that effect and
 * use the runInterpreter method. With the `Interpreter` trait you need to define:
 *
 *  - what to do with pure values
 *  - what to do with an effect
 *  - what to do with a list of effects (the "applicative" case)
 *  - what to do with a "last" effect, in case of having side-effects to finalize resources (see the SafeEffect)
 *
 * For each of those methods you get access to a continuation which you may or may not invoke to create the next
 * effect in a sequence of effects. For example with the EitherEffect once you arrive on a Left value you don't trigger
 * the continuation because there is no value to trigger it with.
 *
 * There are also easier ways to define interpreters. The `recurse` method and the `Recurser` trait define:
 *
 *  - `onPure(a: A): B`: how to map a pure value A to the result B
 *  - `onEffect[X](mx: M[X]): X Either Eff[R, B]`: either extract a value from the effect or return another effect
 *  - `onApplicative[X](tx: T[M[X]]): T[X] Either M[T[X]]`: either extract individual values from each effect or "sequence" the effect
 *
 * Even simpler, the `Translate` trait does a translation from an effect `M[X]` to other effects in the stack.
 *
 * There are also a few `intercept` methods to use an effect but still leave it in the stack
 *
 */
trait Interpret {

  def runInterpreter[R, U, T[_], A, B](e: Eff[R, A])(interpreter: Interpreter[T, U, A, B])(implicit m: Member.Aux[T, R, U]): Eff[U, B] = {

    def interpretContinuation[X](c: Continuation[R, X, A]): Continuation[U, X, B] =
      Continuation.lift((x: X) => runInterpreter(c(x))(interpreter), interpretLast(c.onNone))

    def interpretContinuationWithLast[X](c: Continuation[R, X, A], last: Last[R]): Continuation[U, X, B] =
      Continuation.lift((x: X) => runInterpreter(c(x).addLast(last))(interpreter), interpretLast(c.onNone))

    def interpretLastEff(last: Eff[R, Unit]): Eff[U, Unit] =
      last match {
        case Pure(_, last1) =>
          interpretLast(last1).value.map(_.value).getOrElse(Eff.pure(()))

        case Impure(NoEffect(a), c, last1) =>
          interpretLastEff(c(a).addLast(last1))

        case Impure(u: Union[?, ?], c, last1) =>
          m.project(u) match {
            case Right(tu) =>
              interpreter.onLastEffect(tu, Continuation.lift((x: u.X) => interpretLastEff(c(x).addLast(last1)), interpretLast(c.onNone)))
            case Left(other) => Impure(other, Continuation.lift((x: u.X) => interpretLastEff(c(x)), interpretLast(c.onNone)), interpretLast(last1))
          }

        case ap @ ImpureAp(_, _, _) =>
          interpretLastEff(ap.toMonadic)
      }

    def interpretLast(last: Last[R]): Last[U] =
      last.value match {
        case None => Last.none[U]
        case Some(l) => Last.eff(interpretLastEff(l.value))
      }

    e match {
      case Pure(a, last) =>
        interpreter.onPure(a).addLast(interpretLast(last))

      case Impure(NoEffect(a), c, last) =>
        Impure(NoEffect(a), interpretContinuation(c), interpretLast(last))

      case Impure(u: Union[?, ?], c, last) =>
        m.project(u) match {
          case Right(tu) => interpreter.onEffect(tu, interpretContinuationWithLast(c, last))
          case Left(other) => Impure(other, interpretContinuation(c), interpretLast(last))
        }

      case ImpureAp(unions, continuation, last) =>
        val collected = unions.project

        if (collected.effects.isEmpty)
          collected.othersEff(interpretContinuation(continuation)).addLast(interpretLast(last))
        else
          interpreter.onApplicativeEffect(collected.effects, collected.continuation(interpretContinuation(continuation))).addLast(interpretLast(last))
    }
  }

  /**
   * Interpret an effect with a Recurser
   */
  def recurse[R, U, T[_], A, B](e: Eff[R, A])(recurser: Recurser[T, U, A, B])(implicit m: Member.Aux[T, R, U]): Eff[U, B] =
    runInterpreter(e)(Interpreter.fromRecurser(recurser))

  /**
   * transform an effect into another one
   * using a natural transformation, leaving the rest of the stack untouched
   */
  def transform[SR, BR, U1, U2, TS[_], TB[_], A](effect: Eff[SR, A], nat: TS ~> TB)(implicit
    sr: Member.Aux[TS, SR, U1],
    br: Member.Aux[TB, BR, U2],
    into: IntoPoly[U1, U2]
  ): Eff[BR, A] = {
    val m: Member.Aux[TS, SR, BR] = new Member[TS, SR] {
      type Out = BR

      def inject[V](tv: TS[V]): Union[SR, V] =
        sr.inject(tv)

      def accept[V](union: Union[Out, V]): Union[SR, V] =
        ??? // not used

      def project[V](union: Union[SR, V]): Union[Out, V] Either TS[V] =
        sr.project(union) match {
          case Right(u) => Right(u)
          case Left(o) => Left(br.accept(into.unionInto(o)))
        }

    }

    runInterpreter[SR, BR, TS, A, A](effect)(Interpreter.fromNat[TS, TB, BR, A](nat)(using br))(using m)
  }

  /**
   * Translate one effect of the stack into some of the other effects in the stack
   */
  def translate[R, U, T[_], A](effect: Eff[R, A])(tr: Translate[T, U])(implicit m: Member.Aux[T, R, U]): Eff[U, A] =
    runInterpreter(effect)(Interpreter.fromTranslate(tr))

  /**
   * Translate one effect of the stack into some of the other effects in the stack
   * Using a natural transformation
   */
  def translateNat[R, U, T[_], A](effects: Eff[R, A])(nat: T ~> Eff[U, *])(implicit m: Member.Aux[T, R, U]): Eff[U, A] =
    translate(effects)(new Translate[T, U] {
      def apply[X](tx: T[X]): Eff[U, X] = nat(tx)
    })

  /**
   * Translate one effect of the stack into other effects in a larger stack
   */
  def translateInto[R, T[_], U, A](effect: Eff[R, A])(tr: Translate[T, U])(implicit t: T /= R, into: IntoPoly[R, U]): Eff[U, A] = {
    val m: Member.Aux[T, R, U] = new Member[T, R] {
      type Out = U

      def inject[V](tv: T[V]): Union[R, V] =
        t.inject(tv)

      def accept[V](union: Union[Out, V]): Union[R, V] =
        ??? // not used

      def project[V](union: Union[R, V]): Union[Out, V] Either T[V] =
        t.extract(union) match {
          case Some(u) => Right(u)
          case None => Left(into.unionInto(union))
        }
    }

    translate[R, U, T, A](effect)(tr)(using m)
  }

  /** interpret an effect by running side-effects */
  def interpretUnsafe[R, U, T[_], A](effect: Eff[R, A])(sideEffect: SideEffect[T])(implicit m: Member.Aux[T, R, U]): Eff[U, A] =
    runInterpreter[R, U, T, A, A](effect)(Interpreter.fromSideEffect(sideEffect))

  def intercept[R, T[_], A, B](e: Eff[R, A])(interpreter: Interpreter[T, R, A, B])(implicit m: T /= R): Eff[R, B] =
    runInterpreter[R, R, T, A, B](e)(interpreter)(using m.toMember)

  /**
   * Intercept the values for one effect and transform them into
   * other values for the same effect
   */
  def interceptNat[R, T[_], A](effect: Eff[R, A])(nat: T ~> T)(implicit m: T /= R): Eff[R, A] =
    intercept(effect)(Interpreter.fromNat(nat))

  type of[F[_], G[_]] = { type l[A] = F[G[A]] }

  /**
   * Intercept the values for one effect,
   * emitting new values for the same effect inside a monad which is interleaved in
   */
  def interceptNatM[R, M[_], F[_], A](effect: Eff[R, A], nat: M ~> (M `of` F)#l)(implicit
    m: MemberInOut[M, R],
    FT: Traverse[F],
    FM: Monad[F]
  ): Eff[R, F[A]] =
    intercept[R, M, A, F[A]](effect)(new Interpreter[M, R, A, F[A]] {
      def onPure(a: A): Eff[R, F[A]] =
        Eff.pure(FM.pure(a))

      def onEffect[X](mx: M[X], continuation: Continuation[R, X, F[A]]): Eff[R, F[A]] =
        Impure(m.inject(nat(mx)), Continuation.lift((fx: F[X]) => Eff.flatTraverseA(fx)(continuation), continuation.onNone))

      def onLastEffect[X](mx: M[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] =
        Impure(m.inject(nat(mx)), Continuation.lift((fx: F[X]) => Eff.flatTraverseA(fx)(x => continuation(x).map(FM.pure)).void, continuation.onNone))

      def onApplicativeEffect[X, T[_]: Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], F[A]]): Eff[R, F[A]] = {
        val xss = xs.toList.map(mx => nat(mx)).toVector.map(m.inject)
        ImpureAp(
          Unions(xss.head, xss.tail.asInstanceOf[Vector[Union[R, Any]]]),
          Continuation
            .lift((tfx: Vector[Any]) => FT.map(tfx.asInstanceOf[T[F[X]]].sequence)(continuation).sequence.map(_.flatten), continuation.onNone)
        )
      }

    })

  /**
   * Interpret the effect T with a side-effect O (see the write method below)
   */
  def augment[R, T[_], O[_], A](eff: Eff[R, A])(w: Augment[T, O])(implicit memberT: MemberInOut[T, R], memberO: MemberIn[O, R]): Eff[R, A] = {
    translateInto(eff)(new Translate[T, R] {
      def apply[X](tx: T[X]): Eff[R, X] = send[O, R, Unit](w(tx)) >> send[T, R, X](tx)
    })
  }

  /**
   * For each effect T add some "log statements" O using the Writer effect
   */
  def write[R, T[_], O, A](eff: Eff[R, A])(w: Write[T, O])(implicit memberT: MemberInOut[T, R], memberW: MemberIn[Writer[O, *], R]): Eff[R, A] = {
    augment[R, T, Writer[O, *], A](eff)(new Augment[T, Writer[O, *]] {
      def apply[X](tx: T[X]) = Writer.tell[O](w(tx))
    })
  }

  /**
   * For a single effect T log every value of that effect
   */
  def trace[R, T[_], A](eff: Eff[R, A])(implicit memberT: MemberInOut[T, R], memberW: MemberInOut[Writer[T[?], *], R]): Eff[R, A] =
    write[R, T, T[?], A](eff)(new Write[T, T[?]] {
      def apply[X](tx: T[X]): T[?] = tx
    })

}

object Interpret extends Interpret
