package org.atnos.eff
package addon.scalaz

import scalaz._

object validate extends validate

trait validate {

  def runValidationNel[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[Validate[E, *], R, U]): Eff[U, ValidationNel[E, A]] =
    org.atnos.eff.all.runValidatedNel(r).map(_.fold(ls => Validation.failure(NonEmptyList(ls.head, ls.tail:_*)), Validation.success))

  def runNelDisjunction[R, U, E, A](r: Eff[R, A])(implicit m: Member.Aux[Validate[E, *], R, U]): Eff[U, NonEmptyList[E] \/ A] =
    org.atnos.eff.all.runNel(r).map(_.fold(ls => \/.left(NonEmptyList(ls.head, ls.tail:_*)), \/.right))

  def runMapDisjunction[R, U, E, L : Semigroup, A](r: Eff[R, A])(map: E => L)(implicit m: Member.Aux[Validate[E, *], R, U]): Eff[U, L \/ A] =
    org.atnos.eff.all.runMap(r)(map)(catsSemigroup(Semigroup[L]), m).map(_.fold(\/.left, \/.right))

}
