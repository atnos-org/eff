package org.atnos.eff

import cats._
import cats.syntax.all._
import Eff._
import EffCompat._
import scala.annotation.tailrec

trait EffInterpretation {

  /**
   * base runner for an Eff value having no effects at all
   * the execution is trampolined using Eval
   */
  def run[A](eff: Eff[NoFx, A]): A = {
    def runEval[X](e: Eff[NoFx, X]): Eval[X] =
      e match {
        case Pure(a, Last(None)) =>
          Eval.now(a)

        case Pure(a, Last(Some(l))) =>
          runEval(l.value).as(a)

        case Impure(NoEffect(a), c, Last(None)) =>
          Eval.later(c(a)).flatMap(runEval)

        case Impure(NoEffect(a), c, Last(Some(l))) =>
          Eval.later(c(a)).flatMap(runEval).flatMap(res => runEval(l.value).as(res))

        case other =>
          throw new EffImpossibleException("impossible: cannot run the effects in " + other)
      }

    runEval(eff).value
  }

  /**
   * peel-off the only present effect
   */
  def detach[M[_], R, A, E](eff: Eff[R, A])(implicit monad: MonadError[M, E], m: Member.Aux[M, R, NoFx]): M[A] =
    detachA(Eff.effInto[R, Fx1[M], A](eff))

  /**
   * peel-off the only present effect
   */
  def detach[M[_], A, E](eff: Eff[Fx1[M], A])(implicit monad: MonadError[M, E]): M[A] =
    detachA(eff)

  /**
   * peel-off the only present effect, using an Applicative instance where possible
   */
  def detachA[M[_], R, A, E](eff: Eff[R, A])(implicit monad: MonadError[M, E], applicative: Applicative[M], member: Member.Aux[M, R, NoFx]): M[A] =
    detachA(Eff.effInto[R, Fx1[M], A](eff))(monad, applicative)

  /**
   * peel-off the only present effect, using an Applicative instance where possible
   */
  def detachA[M[_], A, E](eff: Eff[Fx1[M], A])(implicit monad: MonadError[M, E], applicative: Applicative[M]): M[A] =
    Monad[M].tailRecM[Eff[Fx1[M], A], A](eff) {
      case Pure(a, Last(Some(l))) => monad.pure(Left(l.value.as(a)))
      case Pure(a, Last(None)) => monad.pure(Right(a))

      case Impure(NoEffect(a), continuation, last) =>
        monad.pure(Left(continuation(a).addLast(last)))

      case Impure(u: Union[?, ?], continuation, last) =>
        val ta = u.tagged.valueUnsafe.asInstanceOf[M[A]]
        val result: M[Either[Eff[Fx1[M], A], A]] =
          ta.map(x => Left(Impure(NoEffect(x.asInstanceOf[Any]), continuation.cast[Continuation[Fx1[M], Any, A]], last)))

        last match {
          case Last(Some(l)) =>
            monad.handleErrorWith(result)(t => detachA(l.value) *> monad.raiseError(t))

          case Last(None) =>
            result
        }

      case ImpureAp(unions, continuation, last) =>
        val effects = unions.unions.map(_.tagged.valueUnsafe.asInstanceOf[M[Any]])
        val sequenced = Traverse[Vector].sequence(effects)(applicative)

        val result: M[Either[Eff[Fx1[M], A], A]] =
          sequenced.map(xs => Left(Impure(NoEffect(xs), continuation, last)))

        last match {
          case Last(Some(l)) =>
            monad.handleErrorWith(result)(t => detachA(l.value) *> monad.raiseError(t))

          case Last(None) =>
            result
        }
    }

  /**
   * get the pure value if there is no effect
   */
  @tailrec
  final def runPure[R, A](eff: Eff[R, A]): Option[A] =
    eff match {
      case Pure(a, Last(Some(l))) => l.value; Some(a)
      case Pure(a, _) => Some(a)
      case Impure(NoEffect(a), c, l) => runPure(c(a).addLast(l))
      case _ => None
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
    send[M, R, Option[A]](cached.get(cache, key)).flatMap(_.map(Eff.pure[R, A]).getOrElse(memoizeEffectSequence(e, cache, key).map(a => {
      cache.put(key, a); a
    })))

  private def memoizeEffectSequence[R, M[_], A](e: Eff[R, A], cache: Cache, key: AnyRef)(implicit
    member: M /= R,
    cached: SequenceCached[M]
  ): Eff[R, A] = {
    var seqKey = 0
    def incrementSeqKey = { val s = seqKey; seqKey += 1; s }

    interpret.interceptNat[R, M, A](e)(new (M ~> M) {
      def apply[X](fa: M[X]): M[X] = cached.apply(cache, key, incrementSeqKey, fa)
    })

  }

}

object EffInterpretation extends EffInterpretation
