package org.atnos.eff.syntax

import cats.Semigroup
import org.atnos.eff._

object either extends either

trait either {
  implicit final def toEitherEffectOps[R, A](e: Eff[R, A]): EitherEffectOps[R, A] = new EitherEffectOps[R, A](e)
}

final class EitherEffectOps[R, A](val e: Eff[R, A]) extends AnyVal {

  def runEither[E](implicit m: Member[(E Either ?), R]): Eff[m.Out, E Either A] =
    EitherInterpretation.runEither[R, m.Out, E, A](e)(m)

  def runEitherU[E, U](implicit m: Member.Aux[(E Either ?), R, U]): Eff[U, E Either A] =
    EitherInterpretation.runEither(e)(m)

  def runEitherCombine[E, U](implicit m: Member.Aux[(E Either ?), R, U], s: Semigroup[E]): Eff[U, E Either A] =
    EitherInterpretation.runEitherCombine(e)(m, s)

  def attemptEither[E](implicit m: (E Either ?) /= R): Eff[R, E Either A] =
    EitherInterpretation.attemptEither(e)(m)

  def catchLeft[E](handle: E => Eff[R, A])(implicit member: (E Either ?) /= R): Eff[R, A] =
    EitherInterpretation.catchLeft(e)(handle)(member)

  def runEitherCatchLeft[E, U](handle: E => Eff[U, A])(implicit member: Member.Aux[(E Either ?), R, U]): Eff[U, A] =
    EitherInterpretation.runEitherCatchLeft(e)(handle)(member)

  def catchLeftCombine[E](handle: E => Eff[R, A])(implicit member: (E Either ?) /= R, s: Semigroup[E]): Eff[R, A] =
    EitherInterpretation.catchLeftCombine(e)(handle)(member, s)

  def zoomEither[BR, U1, U2, C, B](getter: C => B)(
    implicit m1: Member.Aux[C Either ?, R, U1],
             m2: Member.Aux[B Either ?, BR, U2],
             into: IntoPoly[U1, U2]): Eff[BR, A] =
    EitherInterpretation.zoomEither[R, BR, U1, U2, C, B, A](e, getter)

  def translateEither[U, C, B](getter: C => B)(implicit sr: Member.Aux[C Either ?, R, U], br: (B Either ?) |= U): Eff[U, A] =
    EitherInterpretation.translateEither[R, U, C, B, A](e, getter)

  def localEither[E](modify: E => E)(implicit m: E Either ? /= R): Eff[R, A] =
    EitherInterpretation.localEither[R, E, A](e)(modify)
}

