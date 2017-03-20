package org.atnos.eff

import Eff._

/**
 * Typeclass proving that it is possible to send a tree of effects R into another tree of effects U
 *
 * for example
 *
 *  send[Option1, Fx.fx3[Option1, Option2, Option3], Int](Option1(1)).
 *    into[Fx.fx5[Option1, Option2, Option3, Option4, Option5]]
 *
 *  should work because all the effects of the first stack are present in the second
 *
 * Note: some implicit definitions are probably missing in some cases
 */
trait IntoPoly[R, U] {
  def apply[A](e: Eff[R, A]): Eff[U, A]
}

object IntoPoly extends IntoPolyLower1

trait IntoPolyLower1 extends IntoPolyLower2 {

  implicit def intoNil[R]: IntoPoly[NoFx, R] =
    new IntoPoly[NoFx, R] {
      def apply[A](e: Eff[NoFx, A]) =
        e match {
          case Pure(_, _)                => e.asInstanceOf[Eff[R, A]]
          case Impure(NoEffect(a), c, l) => e.asInstanceOf[Eff[R, A]]
          case _                         => sys.error("impossible NoFx into R is only for pure values")
        }
    }

  implicit def intoSelf[R]: IntoPoly[R, R] =
    new IntoPoly[R, R] { def apply[A](e: Eff[R, A]) = e }

}

trait IntoPolyLower2  extends IntoPolyLower3 {

  implicit def intoAppendL2L[T1[_], T2[_], R]: IntoPoly[FxAppend[Fx1[T2], R], FxAppend[Fx2[T1, T2], R]] =
    new IntoPoly[FxAppend[Fx1[T2], R], FxAppend[Fx2[T1, T2], R]] {
      def apply[A](e: Eff[FxAppend[Fx1[T2], R], A]): Eff[FxAppend[Fx2[T1, T2], R], A] =
        new UnionInto[FxAppend[Fx1[T2], R], FxAppend[Fx2[T1, T2], R]] {
          def apply[X](union: Union[FxAppend[Fx1[T2], R], X]) = union match {
            case UnionAppendL(l)   => UnionAppendL(l.tagged.increment)
            case UnionAppendR(r)   => UnionAppendR(r)
            case UnionTagged(_, _) => sys.error("impossible")
          }
        }.into(e)

    }

  implicit def intoAppendL2R[T1[_], T2[_], R]: IntoPoly[FxAppend[Fx1[T1], R], FxAppend[Fx2[T1, T2], R]] =
    new IntoPoly[FxAppend[Fx1[T1], R], FxAppend[Fx2[T1, T2], R]] {
      def apply[A](e: Eff[FxAppend[Fx1[T1], R], A]): Eff[FxAppend[Fx2[T1, T2], R], A] =
        new UnionInto[FxAppend[Fx1[T1], R], FxAppend[Fx2[T1, T2], R]] {
          def apply[X](union: Union[FxAppend[Fx1[T1], R], X]) = union match {
            case UnionAppendL(l)   => UnionAppendL(l.forget)
            case UnionAppendR(r)   => UnionAppendR(r)
            case UnionTagged(_, _) => sys.error("impossible")
          }
        }.into(e)
    }

  implicit def intoAppendL3L[T1[_], T2[_], T3[_], R]: IntoPoly[FxAppend[Fx2[T2, T3], R], FxAppend[Fx3[T1, T2, T3], R]] =
    new IntoPoly[FxAppend[Fx2[T2, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
      def apply[A](e: Eff[FxAppend[Fx2[T2, T3], R], A]): Eff[FxAppend[Fx3[T1, T2, T3], R], A] =
        new UnionInto[FxAppend[Fx2[T2, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
          def apply[X](union: Union[FxAppend[Fx2[T2, T3], R], X]) = union match {
            case UnionAppendL(l)   => UnionAppendL(l.tagged.increment)
            case UnionAppendR(r)   => UnionAppendR(r)
            case UnionTagged(_, _) => sys.error("impossible")
          }
        }.into(e)
    }

  implicit def intoAppendL3M[T1[_], T2[_], T3[_], R]: IntoPoly[FxAppend[Fx2[T1, T3], R], FxAppend[Fx3[T1, T2, T3], R]] =
    new IntoPoly[FxAppend[Fx2[T1, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
      def apply[A](e: Eff[FxAppend[Fx2[T1, T3], R], A]): Eff[FxAppend[Fx3[T1, T2, T3], R], A] =
        new UnionInto[FxAppend[Fx2[T1, T3], R], FxAppend[Fx3[T1, T2, T3], R]] {
          def apply[X](union: Union[FxAppend[Fx2[T1, T3], R], X]) = union match {
            case UnionAppendL(l)   =>
              if (l.tagged.index == 1) UnionAppendL(l.tagged.forget)
              else                     UnionAppendL(l.tagged.increment)
            case UnionAppendR(r)   => UnionAppendR(r)
            case UnionTagged(_, _) => sys.error("impossible")
          }
        }.into(e)
    }

  implicit def intoAppendL3R[T1[_], T2[_], T3[_], R]: IntoPoly[FxAppend[Fx2[T1, T2], R], FxAppend[Fx3[T1, T2, T3], R]] =
    new IntoPoly[FxAppend[Fx2[T1, T2], R], FxAppend[Fx3[T1, T2, T3], R]] {
      def apply[A](e: Eff[FxAppend[Fx2[T1, T2], R], A]): Eff[FxAppend[Fx3[T1, T2, T3], R], A] =
        new UnionInto[FxAppend[Fx2[T1, T2], R], FxAppend[Fx3[T1, T2, T3], R]] {
          def apply[X](union: Union[FxAppend[Fx2[T1, T2], R], X]) = union match {
            case UnionAppendL(l)   => UnionAppendL(l.forget)
            case UnionAppendR(r)   => UnionAppendR(r)
            case UnionTagged(_, _) => sys.error("impossible")
          }
        }.into(e)
    }
}

trait IntoPolyLower3 extends IntoPolyLower4 {
  implicit def intoAppendL1[T[_], R]: IntoPoly[R, FxAppend[Fx1[T], R]] =
    new IntoPoly[R, FxAppend[Fx1[T], R]] {
      def apply[A](e: Eff[R, A]): Eff[FxAppend[Fx1[T], R], A] =
        new UnionInto[R, FxAppend[Fx1[T], R]] {
          def apply[X](union: Union[R, X]): Union[FxAppend[Fx1[T], R], X] =
            UnionAppendR(union)
        }.into(e)
    }
}

trait IntoPolyLower4 extends IntoPolyLower5 {

  implicit def into[T[_], R, U, S](implicit
                                   t: Member.Aux[T, R, S],
                                   m: T |= U,
                                   recurse: IntoPoly[S, U]): IntoPoly[R, U] =
    new IntoPoly[R, U] {
      def apply[A](e: Eff[R, A]): Eff[U, A] =
        e match {
          case Pure(a, last) =>
            pure(a).addLast(last.interpret(apply))

          case Impure(NoEffect(a), c, l) =>
            apply(c(a).addLast(l))
            
          case Impure(u: Union[_, _], c, l) =>
            t.project(u) match {
              case Right(tx) => Impure[U, u.X, A](m.inject(tx), c.interpretEff(apply)(apply), l.interpret(apply))
              case Left(s)   => recurse(Impure[S, s.X, s.X](s, Arrs.unit)).flatMap((x: Any) => apply(c(x))).addLast(l.interpret(apply))
            }

          case ImpureAp(unions, continuation, l) =>
            ImpureAp[U, unions.X, A](unions.into(new UnionInto[R, U] {
              def apply[X](u: Union[R, X]): Union[U, X] =
                t.project(u) match {
                  case Right(t1)   => m.inject(t1)
                  case Left(other) =>
                    recurse(Impure[S, X, X](other, Arrs.unit)) match {
                      case Impure(u1, _, _) => u1.asInstanceOf[Union[U, X]]
                      case _ => sys.error("impossible into case: Impure must be transformed to Impure")
                    }
                }
            }), continuation.interpretEff(apply)(apply), l.interpret(apply))
        }
    }

}

trait IntoPolyLower5 {

  implicit def intoMember[T[_], R, U](implicit m: Member.Aux[T, R, U]): IntoPoly[U, R] = new IntoPoly[U, R] {
    def apply[A](e: Eff[U, A]): Eff[R, A] =
      e match {
        case Pure(a, last) =>
          pure(a).addLast(last.interpret(apply))

        case Impure(NoEffect(a), c, l) =>
          apply(c(a).addLast(l))

        case Impure(u: Union[_, _], c, l) =>
          Impure(m.accept(u), c.interpretEff(apply)(apply), l.interpret(apply))

        case ImpureAp(unions, c, l) =>
          ImpureAp(unions.into(new UnionInto[U, R] { def apply[X](u: Union[U, X]) = m.accept(u) }),
            c.interpretEff(apply)(apply), l.interpret(apply))
      }
  }
}
