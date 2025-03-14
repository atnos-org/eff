package org.atnos.eff

import cats.*

/**
 * The Safe type is a mix of a ThrowableEither / Eval effect
 *   and a writer effect to collect finalizer failures
 */
sealed trait Safe[A] {
  def memoize: Safe[A]
}

case class EvaluateValue[A](run: Eval[A]) extends Safe[A] {
  def memoize: Safe[A] =
    copy(run.memoize)
}

case class FailedValue[A](t: Throwable) extends Safe[A] {
  def memoize: Safe[A] =
    this
}

case class FailedFinalizer(t: Throwable) extends Safe[Unit] {
  def memoize: Safe[Unit] =
    this
}

object Safe {

  def evaluate[A](a: => A): Safe[A] =
    EvaluateValue[A](Eval.later(a))

  def eval[A](a: Eval[A]): Safe[A] =
    EvaluateValue[A](a)

  def fail[A](t: Throwable): Safe[A] =
    FailedValue(t)

  def failFinalizer(t: Throwable): Safe[Unit] =
    FailedFinalizer(t)

  implicit val safeSequenceCached: SequenceCached[Safe] =
    new SequenceCached[Safe] {
      def get[X](cache: Cache, key: AnyRef): Safe[Option[X]] =
        evaluate(cache.get(key))

      def apply[X](cache: Cache, key: AnyRef, subKey: Int, tx: => Safe[X]): Safe[X] =
        cache.memo((key, subKey), tx.memoize)

      def reset(cache: Cache, key: AnyRef): Safe[Unit] =
        EvaluateValue(Eval.later {
          cache.reset(key)
          var i = 0
          while (cache.get((key, i)).isDefined) {
            cache.reset((key, i))
            i += 1
          }
        })

    }

}
