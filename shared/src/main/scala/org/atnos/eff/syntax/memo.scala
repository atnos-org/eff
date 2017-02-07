package org.atnos.eff.syntax

import cats._
import org.atnos.eff._
import MemoEffect._

import scala.concurrent.Future

object memo extends memo

trait memo {

  implicit def toMemoEffectOps[R, A](e: Eff[R, A]): MemoEffectOps[R, A] = new MemoEffectOps[R, A](e)

}

final class MemoEffectOps[R, A](val e: Eff[R, A]) extends AnyVal {

  def runMemo[U](cache: Cache)(implicit member: Member.Aux[Memoized, R, U], eval: Eval |= U): Eff[U, A] =
    MemoEffect.runMemo(cache)(e)(member, eval)

  def runFutureMemo[U](cache: Cache)(implicit memMember: Member.Aux[Memoized, R, U],
                                                  futMember: TimedFuture |= U): Eff[U, A] =
    MemoEffect.runFutureMemo(cache)(e)(memMember, futMember)

}

