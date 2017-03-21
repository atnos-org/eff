package org.atnos.eff

import cats._
import cats.data._
import cats.implicits._
import Eff._

/**
 * Support methods to create interpreters (or "effect handlers") for a given effect M and a value Eff[R, A]
 * when M is a member of R.
 */
trait Interpret {

  def runInterpreter[R, U, T[_], A, B](e: Eff[R, A])(interpreter: Interpreter[T, U, A, B])
                                                    (implicit m: Member.Aux[T, R, U]): Eff[U, B] = {

    def interpretContinuation[X](c: Continuation[R, X, A]): Continuation[U, X, B] =
      Continuation.lift((x: X) => runInterpreter(c(x))(interpreter), interpretLast(c.onNone))

    def interpretContinuationWithLast[X](c: Continuation[R, X, A], last: Last[R]): Continuation[U, X, B] =
      Continuation.lift((x: X) => runInterpreter(c(x).addLast(last))(interpreter), interpretLast(c.onNone))

    def interpretLastEff(last: Eff[R, Unit]): Eff[U, Unit] =
      last match {
        case Pure(a, last1) =>
          interpretLast(last1).value.map(_.value).getOrElse(Eff.pure(()))

        case Impure(NoEffect(a), c, last1) =>
          interpretLastEff(c(a).addLast(last1))

        case Impure(u: Union[_,_], c, last1) =>
          m.project(u) match {
            case Right(tu)   => interpreter.onLastEffect(tu, Continuation.lift((x: u.X) => interpretLastEff(c(x).addLast(last1)), interpretLast(c.onNone)))
            case Left(other) => Impure(other, Continuation.lift((x: u.X) => interpretLastEff(c(x)), interpretLast(c.onNone)), interpretLast(last1))
          }

        case ap @ ImpureAp(_, _, _) =>
          interpretLastEff(ap.toMonadic)
      }

    def interpretLast(last: Last[R]): Last[U] =
      last.value match {
        case None    => Last.none[U]
        case Some(l) => Last.eff(interpretLastEff(l.value))
      }

    e match {
      case Pure(a, last) =>
        interpreter.onPure(a).addLast(interpretLast(last))

      case Impure(NoEffect(a), c, last) =>
        runInterpreter(c(a).addLast(last))(interpreter)

      case Impure(u: Union[_,_], c, last) =>
        m.project(u) match {
          case Right(tu)   => interpreter.onEffect(tu, interpretContinuationWithLast(c, last))
          case Left(other) => Impure(other, interpretContinuation(c), interpretLast(last))
        }

      case ap @ ImpureAp(unions, continuation, last) =>
        val collected = unions.project

        if (collected.effects.isEmpty)
          collected.othersEff(continuation.interpret(e => runInterpreter(e)(interpreter))(interpretLast)).addLast(interpretLast(last))
        else
          interpreter.onApplicativeEffect(collected.effects, interpretContinuation(continuation)).addLast(interpretLast(last))
    }
  }

  def recurse[R, U, T[_], A, B](e: Eff[R, A])
                               (recurser: Recurser[T, U, A, B])
                               (implicit m: Member.Aux[T, R, U]): Eff[U, B] =
    runInterpreter(e)(Interpreter.fromRecurser(recurser))

  /**
   * transform an effect into another one
   * using a natural transformation, leaving the rest of the stack untouched
   */
  def transform[SR, BR, U, TS[_], TB[_], A](effect: Eff[SR, A], nat: TS ~> TB)
                                               (implicit sr: Member.Aux[TS, SR, U], br: Member.Aux[TB, BR, U]): Eff[BR, A] = {
    val m: Member.Aux[TS, SR, BR] = new Member[TS, SR] {
     type Out = BR

      def inject[V](tv: TS[V]): Union[SR, V] =
        sr.inject(tv)

      def accept[V](union: Union[Out, V]): Union[SR, V] =
        ??? // not used

      def project[V](union: Union[SR, V]): Union[Out, V] Either TS[V] =
        sr.project(union) match {
          case Right(u) => Right(u)
          case Left(o)  => Left(br.accept(o))
        }

    }

    runInterpreter[SR, BR, TS, A, A](effect)(Interpreter.fromNat[TS, TB, BR, A](nat)(br))(m)
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
  def translateNat[R, U, T[_], A](effects: Eff[R, A])(nat: T ~> Eff[U, ?])(implicit m: Member.Aux[T, R, U]): Eff[U, A] =
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
          case None    => Left(into.unionInto(union))
        }
    }

    translate[R, U, T, A](effect)(tr)(m)
  }

  def augment[R, T[_], O[_], A](eff: Eff[R, A])(w: Augment[T, O])(implicit m: MemberInOut[T, R]): Eff[Fx.prepend[O, R], A] =  {
    type U = Fx.prepend[O, R]
    implicit val mw = MemberIn.MemberInAppendAnyL

    translateInto(eff)(new Translate[T, U] {
      def apply[X](tx: T[X]): Eff[U, X] = send[O, U, Unit](w(tx)) >> send[T, U, X](tx)
    })
  }

  def write[R, T[_], O, A](eff: Eff[R, A])(w: Write[T, O])(implicit m: MemberInOut[T, R]): Eff[Fx.prepend[Writer[O, ?], R], A] =  {
    type U = Fx.prepend[Writer[O, ?], R]
    implicit val mw = MemberIn.MemberInAppendAnyL

    augment[R, T, Writer[O, ?], A](eff)(new Augment[T, Writer[O, ?]]{
      def apply[X](tx: T[X]) = Writer.tell[O](w(tx))
    })
  }

  def intercept[R, T[_], A, B](e: Eff[R, A])
                              (interpreter: Interpreter[T, R, A, B])
                              (implicit m: T /= R): Eff[R, B] =
    runInterpreter[R, R, T, A, B](e)(interpreter)(m.toMember)

  /**
   * Intercept the values for one effect and transform them into
   * other values for the same effect
   */
  def interceptNat[R, T[_], A](effect: Eff[R, A])
                              (nat: T ~> T)
                              (implicit m: T /= R): Eff[R, A] =
    intercept(effect)(Interpreter.fromNat(nat))

  type of[F[_], G[_]] = {type l[A] = F[G[A]]}

  /**
   * Intercept the values for one effect,
   * emitting new values for the same effect inside a monad which is interleaved in
   */
  def interceptNatM[R, M[_], F[_], A](effect: Eff[R, A], nat: M ~> (M `of` F)#l)
                                     (implicit m: MemberInOut[M, R], FT: Traverse[F], FM: Monad[F]): Eff[R, F[A]] =
    intercept[R, M, A, F[A]](effect)(new Interpreter[M, R, A, F[A]] {
      def onPure(a: A): Eff[R, F[A]] =
        Eff.pure(FM.pure(a))

      def onEffect[X](mx: M[X], continuation: Continuation[R, X, F[A]]): Eff[R, F[A]] =
        Impure(m.inject(nat(mx)), Continuation.lift((fx: F[X]) => Eff.flatTraverseA(fx)(continuation), continuation.onNone))

      def onLastEffect[X](mx: M[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] =
        Impure(m.inject(nat(mx)), Continuation.lift((fx: F[X]) => Eff.flatTraverseA(fx)(x => continuation(x).map(FM.pure)).void, continuation.onNone))

      def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], F[A]]): Eff[R, F[A]] = {
        val xss = xs.toList.map(mx => nat(mx)).toVector.map(m.inject)
        ImpureAp(Unions(xss.head, xss.tail.asInstanceOf[Vector[Union[R, Any]]]), Continuation.lift((tfx: Vector[Any]) =>
          FT.map(tfx.asInstanceOf[T[F[X]]].sequence)(continuation).sequence.map(_.flatten), continuation.onNone))
      }

    })

  /** interpret an effect by running side-effects */
  def interpretUnsafe[R, U, T[_], A](effect: Eff[R, A])(sideEffect: SideEffect[T])
                                    (implicit m: Member.Aux[T, R, U]): Eff[U, A] =
    runInterpreter[R, U, T, A, A](effect)(Interpreter.fromSideEffect(sideEffect))


}

/**
 * Interpret eff values
 *
 * For stack-safety reasons, the continuation must *never* be called
 * with a value directly, but always with Eff.impure:
 *
 * Eff.impure(a, continuation)
 *
 * * *Note* it is the responsibility of the implementation to call continuation.onNone if
 * the continuation is not used to create the return value.
 */
trait Interpreter[M[_], R, A, B] {

  /**
   * Interpret a pure value
   */
  def onPure(a: A): Eff[R, B]

  /**
   * Interpret an effect of type M
   *
   * if the value X can be extracted call the continuation to get the next Eff[R, B] value
   * otherwise provide a Eff[R, B] value
   */
  def onEffect[X](x: M[X], continuation: Continuation[R, X, B]): Eff[R, B]

  /**
   * Interpret a side-effect of type M
   *
   * if the value X can be extracted call the continuation to get the next Eff[R, B] value
   * otherwise provide a Eff[R, B] value
   */
  def onLastEffect[X](x: M[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit]

  /**
   * Interpret a list of effects of type M
   *
   * if the value X can be extracted call the continuation to get the next Eff[R, B] value
   * otherwise provide a Eff[R, B] value
   */
  def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], B]): Eff[R, B]
}

object Interpreter {

  def fromRecurser[M[_], R, A, B](recurser: Recurser[M, R, A, B]): Interpreter[M, R, A, B] =
    new Interpreter[M, R, A, B] {
      def onPure(a: A): Eff[R, B] =
        Eff.pure(recurser.onPure(a))

      def onLastEffect[X](x: M[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] =
        Eff.pure(())

      def onEffect[X](mx: M[X], continuation: Continuation[R, X, B]): Eff[R, B] =
        recurser.onEffect(mx) match {
          case Left(x)  => Eff.impure(x, continuation)
          case Right(b) => continuation.runOnNone >> b
        }

      def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], B]): Eff[R, B] =
        recurser.onApplicative(xs) match {
          case Left(x)   => Eff.impure(x, continuation)
          case Right(mx) => onEffect(mx, continuation)
        }
    }

  def fromTranslate[M[_], R, A](translate: Translate[M, R]): Interpreter[M, R, A, A] =
    new Interpreter[M, R, A, A] {
      def onPure(a: A): Eff[R, A] =
        Eff.pure(a)

      def onEffect[X](x: M[X], continuation: Continuation[R, X, A]): Eff[R, A] =
        whenStopped(translate(x).flatMap(continuation), continuation.onNone)

      def onLastEffect[X](x: M[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] =
        whenStopped(translate(x).flatMap(continuation), continuation.onNone)

      def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], A]): Eff[R, A] =
        whenStopped(xs.traverse(translate.apply).flatMap(continuation), continuation.onNone)
    }

  def fromNat[M[_], N[_], R, A](nat: M ~> N)(implicit n: N |= R): Interpreter[M, R, A, A] =
    fromTranslate(new Translate[M, R] {
      def apply[X](x: M[X]): Eff[R, X] =
        Eff.send[N, R, X](nat(x))
    })

  def fromSideEffect[M[_], R, A](sideEffect: SideEffect[M]): Interpreter[M, R, A, A] =
    fromRecurser[M, R, A, A](new Recurser[M, R, A, A] {
      def onPure(a: A): A =
        a

      def onEffect[X](mx: M[X]): X Either Eff[R, A] =
        Left(sideEffect(mx))

      def onApplicative[X, T[_]: Traverse](ms: T[M[X]]): T[X] Either M[T[X]] =
        Left(ms.map(sideEffect.apply))
    })
}

trait Recurser[M[_], R, A, B] {
  def onPure(a: A): B
  def onEffect[X](m: M[X]): X Either Eff[R, B]
  def onApplicative[X, T[_]: Traverse](ms: T[M[X]]): T[X] Either M[T[X]]
}


object Interpret extends Interpret

/**
 * Helper trait for computations
 * which might produce several M[X] in a stack of effects.
 *
 * Either we can produce an X to pass to a continuation or we're done
 *
 * For the applicative case we expect to be able to traverse a list
 * of effects and return an effect of a list of results OR
 * completely consume the effect and return a pure list of values
 */
trait Recurse[M[_], R, A] {
  def apply[X](m: M[X]): X Either Eff[R, A]
  def applicative[X, T[_]: Traverse](ms: T[M[X]]): T[X] Either M[T[X]]
}

/**
 * Generalisation of Recurse and StateRecurse
 *
 * The loop defines some state with an initial value which is maintained at
 * each step of the interpretation.
 *
 * A is the type of Eff values to interpret, and B is the result of the
 * interpretation (generally an other Eff value)
 *
 * C is the type of result for "last" actions.
 *
 * - the interpretation of a Pure value either returns the final result or possibly
 *   one more Eff value to interpret
 *
 * - onEffect interprets one effect and possibly uses the continuation to produce the next
 *   value to interpret. If no X can be used to run the continuation we might just
 *   output one final B value
 *
 *  - onLastEffect interprets the last effect of an Eff value. The only difference with onEffect
 *    is the fact that last actions return Unit values (and not A values)
 *
 *  - onApplicativeEff interprets a list of effects and possibly uses the continuation to
 *    get to the next value to interpret. If no interpretation can be done, a B value might be returned
 *
 *  - onLastApplicativeEffect does the same thing for last actions
 *
 */
trait Loop[M[_], R, A, B, C] {
  type S
  val init: S

  def onPure(a: A, s: S): (Eff[R, A], S) Either B

  def onEffect[X](x: M[X], continuation: Continuation[R, X, A], s: S): (Eff[R, A], S) Either B
  def onLastEffect[X](x: M[X], continuation: Continuation[R, X, Unit], s: S): (Eff[R, Unit], S) Either C

  def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], A], s: S): (Eff[R, A], S) Either B
  def onLastApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], Unit], s: S): (Eff[R, Unit], S) Either C
}

/**
 * Generalisation of Recurse
 */
trait StatelessLoop[M[_], R, A, B, C] {
  def onPure(a: A): Eff[R, A] Either B

  def onEffect[X](x: M[X], continuation: Continuation[R, X, A]): Eff[R, A] Either B
  def onLastEffect[X](x: M[X], continuation: Continuation[R, X, Unit]): Eff[R, Unit] Either C

  def onApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], A]): Eff[R, A] Either B
  def onLastApplicativeEffect[X, T[_] : Traverse](xs: T[M[X]], continuation: Continuation[R, T[X], Unit]): Eff[R, Unit] Either C
}

/**
 * trait for translating one effect into other ones in the same stack
 */
trait Translate[T[_], U] {
  def apply[X](kv: T[X]): Eff[U, X]
}

trait SideEffect[T[_]] {
  def apply[X](tx: T[X]): X
  def applicative[X, Tr[_] : Traverse](ms: Tr[T[X]]): Tr[X]
}

trait Augment[T[_], O[_]] {
  def apply[X](tx: T[X]): O[Unit]
}

trait Write[T[_], O] {
  def apply[X](tx: T[X]): O
}

