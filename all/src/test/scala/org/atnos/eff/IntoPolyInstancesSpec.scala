package org.atnos.eff

import org.specs2.Specification

class IntoPolyInstancesSpec extends Specification {
  def is = s2"""

 The IntoPoly implicit definitions must create lawful instances:

   intoAppendL2L   $intoAppendL2L
   intoAppendL2R   $intoAppendL2R
   intoAppendL3L   $intoAppendL3L
   intoAppendL3M   $intoAppendL3M
   intoAppendL3R   $intoAppendL3R
   intoAppendL1    $intoAppendL1
   intoNoFxAppendL $intoNoFxAppendL
   intoNoFxAppendR $intoNoFxAppendR

"""

  def intoAppendL2L = {
    val into = IntoPoly.intoAppendL2L[T1, T2, Fx2[T3, T4]]
    checkLaw(into, Eff.send[T2, Fx.prepend[T2, Fx2[T3, T4]], Int](t2), Eff.send[T2, Fx.append[Fx2[T1, T2], Fx2[T3, T4]], Int](t2))
    checkLaw(into, Eff.send[T3, Fx.prepend[T2, Fx2[T3, T4]], Int](t3), Eff.send[T3, Fx.append[Fx2[T1, T2], Fx2[T3, T4]], Int](t3))
    checkLaw(into, Eff.send[T4, Fx.prepend[T2, Fx2[T3, T4]], Int](t4), Eff.send[T4, Fx.append[Fx2[T1, T2], Fx2[T3, T4]], Int](t4))
  }

  def intoAppendL2R = {
    val into = IntoPoly.intoAppendL2R[T1, T2, Fx2[T3, T4]]
    checkLaw(into, Eff.send[T1, Fx.prepend[T1, Fx2[T3, T4]], Int](t1), Eff.send[T1, Fx.append[Fx2[T1, T2], Fx2[T3, T4]], Int](t1))
    checkLaw(into, Eff.send[T3, Fx.prepend[T1, Fx2[T3, T4]], Int](t3), Eff.send[T3, Fx.append[Fx2[T1, T2], Fx2[T3, T4]], Int](t3))
    checkLaw(into, Eff.send[T4, Fx.prepend[T1, Fx2[T3, T4]], Int](t4), Eff.send[T4, Fx.append[Fx2[T1, T2], Fx2[T3, T4]], Int](t4))
  }

  def intoAppendL3L = {
    val into = IntoPoly.intoAppendL3L[T1, T2, T3, Fx1[T4]]
    checkLaw(into, Eff.send[T2, Fx.append[Fx2[T2, T3], Fx1[T4]], Int](t2), Eff.send[T2, Fx.append[Fx3[T1, T2, T3], Fx1[T4]], Int](t2))
    checkLaw(into, Eff.send[T3, Fx.append[Fx2[T2, T3], Fx1[T4]], Int](t3), Eff.send[T3, Fx.append[Fx3[T1, T2, T3], Fx1[T4]], Int](t3))
    checkLaw(into, Eff.send[T4, Fx.append[Fx2[T2, T3], Fx1[T4]], Int](t4), Eff.send[T4, Fx.append[Fx3[T1, T2, T3], Fx1[T4]], Int](t4))
  }

  def intoAppendL3M = {
    val into = IntoPoly.intoAppendL3M[T1, T2, T3, Fx1[T4]]
    checkLaw(into, Eff.send[T1, Fx.append[Fx2[T1, T3], Fx1[T4]], Int](t1), Eff.send[T1, Fx.append[Fx3[T1, T2, T3], Fx1[T4]], Int](t1))
    checkLaw(into, Eff.send[T3, Fx.append[Fx2[T1, T3], Fx1[T4]], Int](t3), Eff.send[T3, Fx.append[Fx3[T1, T2, T3], Fx1[T4]], Int](t3))
    checkLaw(into, Eff.send[T4, Fx.append[Fx2[T1, T3], Fx1[T4]], Int](t4), Eff.send[T4, Fx.append[Fx3[T1, T2, T3], Fx1[T4]], Int](t4))
  }

  def intoAppendL3R = {
    val into = IntoPoly.intoAppendL3R[T1, T2, T3, Fx1[T4]]
    checkLaw(into, Eff.send[T1, Fx.append[Fx2[T1, T2], Fx1[T4]], Int](t1), Eff.send[T1, Fx.append[Fx3[T1, T2, T3], Fx1[T4]], Int](t1))
    checkLaw(into, Eff.send[T2, Fx.append[Fx2[T1, T2], Fx1[T4]], Int](t2), Eff.send[T2, Fx.append[Fx3[T1, T2, T3], Fx1[T4]], Int](t2))
    checkLaw(into, Eff.send[T4, Fx.append[Fx2[T1, T2], Fx1[T4]], Int](t4), Eff.send[T4, Fx.append[Fx3[T1, T2, T3], Fx1[T4]], Int](t4))
  }

  def intoAppendL1 = {
    val into = IntoPoly.intoAppendL1[T1, Fx2[T2, T3]]
    checkLaw(into, Eff.send[T2, Fx2[T2, T3], Int](t2), Eff.send[T2, Fx.append[Fx1[T1], Fx2[T2, T3]], Int](t2))
    checkLaw(into, Eff.send[T3, Fx2[T2, T3], Int](t3), Eff.send[T3, Fx.append[Fx1[T1], Fx2[T2, T3]], Int](t3))
  }

  def intoNoFxAppendL = {
    val into = IntoPoly.intoNoFxAppendL[Fx1[T1]]
    checkLaw(into, Eff.send[T1, FxAppend[NoFx, Fx1[T1]], Int](t1), Eff.send[T1, Fx1[T1], Int](t1))
  }

  def intoNoFxAppendR = {
    val into = IntoPoly.intoNoFxAppendR[Fx1[T1]]
    checkLaw(into, Eff.send[T1, FxAppend[Fx1[T1], NoFx], Int](t1), Eff.send[T1, Fx1[T1], Int](t1))
  }

  /**
   * HELPERS
   */

  def checkLaw[R, U, A](into: IntoPoly[R, U], e: Eff[R, A], expected: Eff[U, A]) =
    unions(into.apply(e)) ==== unions(expected)

  def unions[R, A](e: Eff[R, A]): List[Any] =
    e match {
      case Pure(_, _) => List()
      case Impure(u, _, _) => List(u)
      case ImpureAp(u, _, _) => List(u.unions.toList)
    }

  val t1 = T1(1)
  val t2 = T2(1)
  val t3 = T3(1)
  val t4 = T4(1)

  val union1 = Union.one(t1)

  type S1 = Fx.fx1[T1]
  type S2 = Fx.fx2[T1, T2]

  val unionS2_1: Union[S2, Int] = Union.twoL(t1)
  val unionS2_2: Union[S2, Int] = Union.twoR(t2)

  type S3 = Fx.fx3[T1, T2, T3]

  val unionS3_1: Union[S3, Int] = Union.threeL(t1)
  val unionS3_2: Union[S3, Int] = Union.threeM(t2)
  val unionS3_3: Union[S3, Int] = Union.threeR(t3)

  type S4 = Fx.fx4[T1, T2, T3, T4]

  val unionS4_1: Union[S4, Int] = Union.appendL[Fx1[T1], Fx3[T2, T3, T4], Int](union1)
  val unionS4_2: Union[S4, Int] = Union.appendR[Fx1[T1], Fx3[T2, T3, T4], Int](Union.threeL(t2))
  val unionS4_3: Union[S4, Int] = Union.appendR[Fx1[T1], Fx3[T2, T3, T4], Int](Union.threeM(t3))
  val unionS4_4: Union[S4, Int] = Union.appendR[Fx1[T1], Fx3[T2, T3, T4], Int](Union.threeR(t4))

  case class T1[A](a: A)
  case class T2[A](a: A)
  case class T3[A](a: A)
  case class T4[A](a: A)
  case class T5[A](a: A)
}
