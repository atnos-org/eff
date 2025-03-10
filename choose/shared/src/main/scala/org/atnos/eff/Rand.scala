package org.atnos.eff

import cats.*
import scala.util.Random

/**
 * This class can be used as a F in runChoose
 * to generate random alternatives
 */
case class Rand[A](run: Random => Option[A])

object Rand {
  def none[A]: Rand[A] =
    Rand(_ => None)

  given MonadCombineRandom: Alternative[Rand] = new Alternative[Rand] with Monad[Rand] {
    def pure[A](x: A): Rand[A] = Rand(_ => Option(x))

    def empty[A]: Rand[A] = Rand.none[A]

    def combineK[A](x: Rand[A], y: Rand[A]): Rand[A] =
      Rand { r =>
        if (r.nextBoolean()) {
          x.run(r) orElse y.run(r)
        } else {
          y.run(r) orElse x.run(r)
        }
      }

    def flatMap[A, B](fa: Rand[A])(f: A => Rand[B]): Rand[B] =
      Rand[B](rand => fa.run(rand).flatMap(f(_).run(rand)))

    def tailRecM[A, B](a: A)(f: A => Rand[Either[A, B]]): Rand[B] =
      Rand[B] { random =>
        Monad[Option].tailRecM(a)(f(_).run(random))
      }
  }

}
