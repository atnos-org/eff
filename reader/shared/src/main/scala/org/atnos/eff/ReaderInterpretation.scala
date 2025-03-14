package org.atnos.eff

import cats.*
import cats.data.*
import cats.syntax.all.*
import org.atnos.eff.Eff.send
import org.atnos.eff.Interpret.*

trait ReaderInterpretation {

  /**
   * interpret the Reader effect by providing an environment when required
   */
  def runReader[R, U, A, B](env: A)(effect: Eff[R, B])(implicit m: Member.Aux[Reader[A, *], R, U]): Eff[U, B] =
    recurse(effect)(new Recurser[Reader[A, *], U, B, B] {
      def onPure(b: B): B =
        b

      def onEffect[X](r: Reader[A, X]): X Either Eff[U, B] =
        Left(r.run(env))

      def onApplicative[X, T[_]: Traverse](ms: T[Reader[A, X]]): T[X] Either Reader[A, T[X]] =
        Left(ms.map(_.run(env)))
    })

  /**
   * interpret the Kleisli effect by providing an environment when required and translating the
   * resulting target effect into the same stack
   */
  def runKleisli[R, U, S, A, F[_]](env: S)(e: Eff[R, A])(implicit mx: Member.Aux[Kleisli[F, S, *], R, U], m: F |= U): Eff[U, A] =
    interpret.translate(e) {
      new Translate[Kleisli[F, S, *], U] {
        override def apply[X](kv: Kleisli[F, S, X]): Eff[U, X] = {
          send[F, U, X](kv.run(env))
        }
      }
    }

  /**
   * Interpret a Reader effect by using another Reader effect in the same stack
   */
  def translateReader[R, U, S, B, A](e: Eff[R, A], getter: B => S)(implicit sr: Member.Aux[Reader[S, *], R, U], br: Reader[B, *] |= U): Eff[U, A] =
    translate(e) {
      new Translate[Reader[S, *], U] {
        def apply[X](r: Reader[S, X]): Eff[U, X] =
          send[Reader[B, *], U, X](Reader((b: B) => r.run(getter(b))))
      }
    }

  /**
   * Modify the type of the read value
   *
   * This changes the stack of the Eff computation
   */
  def zoomReader[R1, R2, U, S, T, A](
    e: Eff[R1, A]
  )(f: T => S)(implicit readerS: Member.Aux[Reader[S, *], R1, U], readerT: Member.Aux[Reader[T, *], R2, U]): Eff[R2, A] =
    transform(
      e,
      new ~>[Reader[S, *], Reader[T, *]] {
        def apply[X](r: Reader[S, X]): Reader[T, X] =
          Reader((t: T) => r.run(f(t)))
      }
    )

  /**
   * Update the read value, the stack of the Eff computation stays the same
   */
  def localReader[R, T, A](e: Eff[R, A])(modify: T => T)(implicit r: Reader[T, *] /= R): Eff[R, A] =
    interceptNat(e)(new ~>[Reader[T, *], Reader[T, *]] {
      def apply[X](r: Reader[T, X]): Reader[T, X] =
        Reader((t: T) => r.run(modify(t)))
    })
}

object ReaderInterpretation extends ReaderInterpretation
