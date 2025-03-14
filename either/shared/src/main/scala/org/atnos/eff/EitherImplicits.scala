package org.atnos.eff

import cats.*
import cats.syntax.either.*

trait EitherImplicits {

  implicit final def errorTranslate[R, E1, E2](implicit m: MemberIn[Either[E1, *], R], map: E2 => E1): MemberIn[Either[E2, *], R] =
    m.transform(using errorTranslateNat(map))

  final def errorTranslateNat[E1, E2](map: E2 => E1): Either[E2, *] ~> Either[E1, *] = new (Either[E2, *] ~> Either[E1, *]) {
    def apply[X](x2: Either[E2, X]): Either[E1, X] = x2.leftMap(map)
  }

}

object EitherImplicits extends EitherImplicits
