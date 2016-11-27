package org.atnos.eff
package syntax

import cats._
import cats.data.Writer

/**
  * Operations of Eff[R, A] values
  */
object eff extends eff

trait eff extends effOperations with effCats

trait effOperations {
  implicit final def toEffOps[R, A](e: Eff[R, A]): EffOps[R, A] = new EffOps(e)
  implicit final def toEffTranslateIntoOps[R, A](e: Eff[R, A]): EffTranslateIntoOps[R, A] = new EffTranslateIntoOps(e)
  implicit final def toEffNoEffectOps[A](e: Eff[NoFx, A]): EffNoEffectOps[A] = new EffNoEffectOps(e)
  implicit final def toEffSendOps[M[_], A](ma: M[A]): EffSendOps[M, A] = new EffSendOps(ma)
  implicit final def toEffPureOps[A](a: A): EffPureOps[A] = new EffPureOps(a)
  implicit final def toEffOnePureValueOps[R, A](e: Eff[R, A]): EffOnePureValueOps[R, A] = new EffOnePureValueOps(e)
}

trait effCats {
  implicit final def toEffOneEffectOps[M[_], A](e: Eff[Fx1[M], A]): EffOneEffectOps[M, A] = new EffOneEffectOps(e)
  implicit final def toEffMonadicOps[R, M[_], A](e: Eff[R, M[A]]): EffMonadicOps[R, M, A] = new EffMonadicOps(e)
  implicit final def toEffApplicativeOps[F[_], A](values: F[A]): EffApplicativeOps[F, A] = new EffApplicativeOps(values)
  implicit final def toEffSequenceOps[F[_], R, A](values: F[Eff[R, A]]): EffSequenceOps[F, R, A] = new EffSequenceOps(values)
  implicit final def toEffFlatSequenceOps[F[_], R, A](values: F[Eff[R, F[A]]]): EffFlatSequenceOps[F, R, A] = new EffFlatSequenceOps(values)
  implicit final def toEffApplicativeSyntaxOps[R, A](a: Eff[R, A]): EffApplicativeSyntaxOps[R, A] = new EffApplicativeSyntaxOps(a)
}

final class EffOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def into[U](implicit f: IntoPoly[R, U]): Eff[U, A] =
    Eff.effInto(e)(f)

  def transform[BR, U, M[_], N[_]](t: ~>[M, N])(implicit m: Member.Aux[M, R, U], n: Member.Aux[N, BR, U]): Eff[BR, A] =
    Interpret.transform(e, t)(m, n)

  def translate[M[_], U](t: Translate[M, U])(implicit m: Member.Aux[M, R, U]): Eff[U, A] =
    Interpret.translate(e)(t)(m)
}

final class EffTranslateIntoOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def translateInto[T[_], U](t: Translate[T, U])(implicit m: MemberInOut[T, R], into: IntoPoly[R, U]): Eff[U, A] =
    interpret.translateInto(e)(t)(m, into)

  def write[T[_], O](w: Write[T, O])(implicit m: MemberInOut[T, R]): Eff[Fx.prepend[Writer[O, ?], R], A] =
    interpret.write(e)(w)

  def augment[T[_], O[_]](w: Augment[T, O])(implicit m: MemberInOut[T, R]): Eff[Fx.prepend[O, R], A] =
    interpret.augment(e)(w)
}

final class EffNoEffectOps[A](val e: Eff[NoFx, A]) extends AnyVal {
  def run: A =
    Eff.run(e)
}

final class EffSendOps[M[_], A](val ma: M[A]) extends AnyVal {
  def send[R](implicit m: M |= R): Eff[R, A] = Eff.send(ma)
}

final class EffPureOps[A](val a: A) extends AnyVal {
  def pureEff[R]: Eff[R, A] =
    Eff.pure(a)
}

final class EffOneEffectOps[M[_], A](val e: Eff[Fx1[M], A]) extends AnyVal {
  def detach(implicit M: Monad[M]): M[A] =
    Eff.detach(e)

  def detachA(applicative: Applicative[M])(implicit monad: Monad[M]): M[A] =
    Eff.detachA(e)(monad, applicative)
}

final class EffOnePureValueOps[R, A](val e: Eff[R, A]) extends AnyVal {
  def runPure: Option[A] =
    Eff.runPure(e)
}

final class EffMonadicOps[R, M[_], A](val e: Eff[R, M[A]]) extends AnyVal {
  def collapse(implicit m: M |= R): Eff[R, A] =
    Eff.collapse[R, M, A](e)
}

final class EffApplicativeOps[F[_], A](val values: F[A]) extends AnyVal {
  def traverseA[R, B](f: A => Eff[R, B])(implicit F: Traverse[F]): Eff[R, F[B]] =
    Eff.traverseA(values)(f)

  def flatTraverseA[R, B](f: A => Eff[R, F[B]])(implicit F1: Traverse[F], F2: FlatMap[F]): Eff[R, F[B]] =
    Eff.flatTraverseA(values)(f)
}

final class EffSequenceOps[F[_], R, A](val values: F[Eff[R, A]]) extends AnyVal {
  def sequenceA(implicit F: Traverse[F]): Eff[R, F[A]] =
    Eff.sequenceA(values)
}

final class EffFlatSequenceOps[F[_], R, A](val values: F[Eff[R, F[A]]]) extends AnyVal {
  def flatSequenceA(implicit F1: Traverse[F], F2: FlatMap[F]): Eff[R, F[A]] =
    Eff.flatSequenceA(values)
}

final class EffApplicativeSyntaxOps[R, A](val a: Eff[R, A]) extends AnyVal {
  def tuple2[B](b: Eff[R, B]): Eff[R, (A, B)] =
    Eff.EffApplicative[R].tuple2(a, b)

}
