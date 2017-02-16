package org.atnos.eff

import org.specs2.Specification
import org.specs2.matcher.ThrownExpectations

class MemberInstancesSpec extends Specification with ThrownExpectations { def is = s2"""

 The MemberInOut and Member implicit definitions must create lawful instances:

   for m: MemberInOut  m.extract(m.send(ta)) === Option(ta)
   for m: Member       m.project(union).fold(m.accept, m.inject) ==== union

 MemberInOut instances
 =====================

  MemberInOutOut1    $memberInOutOut1
  MemberInOut2L      $memberInOut2L
  MemberInOut3L      $memberInOut3L
  MemberInOutAppendL $memberInOutAppendL

 Member instances
 ================

 Member1        $member1
 Member2L       $member2L
 Member3L       $member3L
 Member4L       $member4L
 Member4RL      $member4RL
 Member4RM      $member4RM
 Member4RR      $member4RR
 MemberAppend1R $memberAppend1R
 MemberAppendL  $memberAppendL
 Member2R       $member2R
 Member3M       $member3M
 MemberAppendR  $memberAppendR
 Member3R       $member3R

"""

  def memberInOutOut1    = checkMemberInOutLaw(MemberInOut.MemberInOutOut1[T1], t1)
  def memberInOut2L      = checkMemberInOutLaw(MemberInOut.MemberInOut2L[T1, T2], t1)
  def memberInOut3L      = checkMemberInOutLaw(MemberInOut.MemberInOut3L[T1, T2, T3], t1)
  def memberInOutAppendL = checkMemberInOutLaw(MemberInOut.MemberInOutAppendL[T1, Fx1[T1], Fx1[T3]], t1)

  def member1 = checkMemberLaw(Member.Member1[T1], union1)

  def member2L = {
    Member.Member2L[T1, T2].project(unionS2_1) must beRight(t1)
    Member.Member2L[T1, T2].project(unionS2_2) must beLeft(Union.one[T2, Int](t2))

    checkMemberLaw(Member.Member2L[T1, T2], unionS2_1)
    checkMemberLaw(Member.Member2L[T1, T2], unionS2_2)
  }

  def member3L = {
    Member.Member3L[T1, T2, T3].project(unionS3_1) must beRight(t1)
    Member.Member3L[T1, T2, T3].project(unionS3_2) must beLeft(Union.twoL[T2, T3, Int](t2))
    Member.Member3L[T1, T2, T3].project(unionS3_3) must beLeft(Union.twoR[T2, T3, Int](t3))

    checkMemberLaw(Member.Member3L[T1, T2, T3], unionS3_1)
    checkMemberLaw(Member.Member3L[T1, T2, T3], unionS3_2)
    checkMemberLaw(Member.Member3L[T1, T2, T3], unionS3_3)
  }

  def member4L = {
    Member.Member4L[T1, T2, T3, T4].project(unionS4_1) must beRight(t1)
    Member.Member4L[T1, T2, T3, T4].project(unionS4_2) must beLeft(Union.threeL[T2, T3, T4, Int](t2))
    Member.Member4L[T1, T2, T3, T4].project(unionS4_3) must beLeft(Union.threeM[T2, T3, T4, Int](t3))
    Member.Member4L[T1, T2, T3, T4].project(unionS4_4) must beLeft(Union.threeR[T2, T3, T4, Int](t4))

    checkMemberLaw(Member.Member4L[T1, T2, T3, T4], unionS4_1)
    checkMemberLaw(Member.Member4L[T1, T2, T3, T4], unionS4_2)
    checkMemberLaw(Member.Member4L[T1, T2, T3, T4], unionS4_3)
    checkMemberLaw(Member.Member4L[T1, T2, T3, T4], unionS4_4)
  }

  def member4RL = {
    Member.Member4RL[T1, T2, T3, T4].project(unionS4_1) must beLeft(Union.threeL[T1, T3, T4, Int](t1))
    Member.Member4RL[T1, T2, T3, T4].project(unionS4_2) must beRight(t2)
    Member.Member4RL[T1, T2, T3, T4].project(unionS4_3) must beLeft(Union.threeM[T1, T3, T4, Int](t3))
    Member.Member4RL[T1, T2, T3, T4].project(unionS4_4) must beLeft(Union.threeR[T1, T3, T4, Int](t4))

    checkMemberLaw(Member.Member4RL[T1, T2, T3, T4], unionS4_1)
    checkMemberLaw(Member.Member4RL[T1, T2, T3, T4], unionS4_2)
    checkMemberLaw(Member.Member4RL[T1, T2, T3, T4], unionS4_3)
    checkMemberLaw(Member.Member4RL[T1, T2, T3, T4], unionS4_4)
  }

  def member4RM = {
    Member.Member4RM[T1, T2, T3, T4].project(unionS4_1) must beLeft(Union.threeL[T1, T2, T4, Int](t1))
    Member.Member4RM[T1, T2, T3, T4].project(unionS4_2) must beLeft(Union.threeM[T1, T2, T4, Int](t2))
    Member.Member4RM[T1, T2, T3, T4].project(unionS4_3) must beRight(t3)
    Member.Member4RM[T1, T2, T3, T4].project(unionS4_4) must beLeft(Union.threeR[T1, T2, T4, Int](t4))

    checkMemberLaw(Member.Member4RM[T1, T2, T3, T4], unionS4_1)
    checkMemberLaw(Member.Member4RM[T1, T2, T3, T4], unionS4_2)
    checkMemberLaw(Member.Member4RM[T1, T2, T3, T4], unionS4_3)
    checkMemberLaw(Member.Member4RM[T1, T2, T3, T4], unionS4_4)
  }

  def member4RR = {
    Member.Member4RR[T1, T2, T3, T4].project(unionS4_1) must beLeft(Union.threeL[T1, T2, T3, Int](t1))
    Member.Member4RR[T1, T2, T3, T4].project(unionS4_2) must beLeft(Union.threeM[T1, T2, T3, Int](t2))
    Member.Member4RR[T1, T2, T3, T4].project(unionS4_3) must beLeft(Union.threeR[T1, T2, T3, Int](t3))
    Member.Member4RR[T1, T2, T3, T4].project(unionS4_4) must beRight(t4)

    checkMemberLaw(Member.Member4RR[T1, T2, T3, T4], unionS4_1)
    checkMemberLaw(Member.Member4RR[T1, T2, T3, T4], unionS4_2)
    checkMemberLaw(Member.Member4RR[T1, T2, T3, T4], unionS4_3)
    checkMemberLaw(Member.Member4RR[T1, T2, T3, T4], unionS4_4)
  }

  def memberAppend1R = {
    Member.MemberAppend1R[T1, Fx3[T2, T3, T4]].project(unionS4_1) must beRight(t1)
    Member.MemberAppend1R[T1, Fx3[T2, T3, T4]].project(unionS4_2) must beLeft(Union.threeL[T2, T3, T4, Int](t2))
    Member.MemberAppend1R[T1, Fx3[T2, T3, T4]].project(unionS4_3) must beLeft(Union.threeM[T2, T3, T4, Int](t3))
    Member.MemberAppend1R[T1, Fx3[T2, T3, T4]].project(unionS4_4) must beLeft(Union.threeR[T2, T3, T4, Int](t4))

    checkMemberLaw(Member.MemberAppend1R[T1, Fx3[T2, T3, T4]], unionS4_1)
    checkMemberLaw(Member.MemberAppend1R[T1, Fx3[T2, T3, T4]], unionS4_2)
    checkMemberLaw(Member.MemberAppend1R[T1, Fx3[T2, T3, T4]], unionS4_3)
    checkMemberLaw(Member.MemberAppend1R[T1, Fx3[T2, T3, T4]], unionS4_4)
  }

  def memberAppendL = {
    Member.MemberAppendL[T1, Fx2[T1, T2], Fx2[T3, T4], Fx1[T2]].project(Union.appendL(Union.twoL(t1))) must beRight(t1)
    Member.MemberAppendL[T1, Fx2[T1, T2], Fx2[T3, T4], Fx1[T2]].project(Union.appendL(Union.twoR(t2))) must beLeft(Union.appendL[Fx1[T2], Fx2[T3, T4], Int](Union.one(t2)))
    Member.MemberAppendL[T1, Fx2[T1, T2], Fx2[T3, T4], Fx1[T2]].project(Union.appendR(Union.twoL(t3))) must beLeft(Union.appendR[Fx1[T2], Fx2[T3, T4], Int](Union.twoL(t3)))
    Member.MemberAppendL[T1, Fx2[T1, T2], Fx2[T3, T4], Fx1[T2]].project(Union.appendR(Union.twoR(t4))) must beLeft(Union.appendR[Fx1[T2], Fx2[T3, T4], Int](Union.twoR(t4)))

    checkMemberLaw(Member.MemberAppendL[T1, Fx1[T1], Fx3[T2, T3, T4], NoFx], unionS4_1)
    checkMemberLaw(Member.MemberAppendL[T1, Fx1[T1], Fx3[T2, T3, T4], NoFx], unionS4_2)
    checkMemberLaw(Member.MemberAppendL[T1, Fx1[T1], Fx3[T2, T3, T4], NoFx], unionS4_3)
    checkMemberLaw(Member.MemberAppendL[T1, Fx1[T1], Fx3[T2, T3, T4], NoFx], unionS4_4)
  }

  def member2R = {
    Member.MemberAppend2R[T1, T2, Fx2[T3, T4]].project(Union.appendL(Union.twoL(t1))) must beLeft(Union.appendL[Fx1[T1], Fx2[T3, T4], Int](Union.one(t1)))
    Member.MemberAppend2R[T1, T2, Fx2[T3, T4]].project(Union.appendL(Union.twoR(t2))) must beRight(t2)
    Member.MemberAppend2R[T1, T2, Fx2[T3, T4]].project(Union.appendR(Union.twoL(t3))) must beLeft(Union.appendR[Fx1[T1], Fx2[T3, T4], Int](Union.twoL(t3)))
    Member.MemberAppend2R[T1, T2, Fx2[T3, T4]].project(Union.appendR(Union.twoR(t4))) must beLeft(Union.appendR[Fx1[T1], Fx2[T3, T4], Int](Union.twoR(t4)))

    checkMemberLaw(Member.Member2R[T1, T2], unionS2_1)
    checkMemberLaw(Member.Member2R[T1, T2], unionS2_2)
  }

  def member3M = {
    Member.Member3M[T1, T2, T3].project(unionS3_1) must beLeft(Union.twoL[T1, T3, Int](t1))
    Member.Member3M[T1, T2, T3].project(unionS3_2) must beRight(t2)
    Member.Member3M[T1, T2, T3].project(unionS3_3) must beLeft(Union.twoR[T1, T3, Int](t3))

    checkMemberLaw(Member.Member3M[T1, T2, T3], unionS3_1)
    checkMemberLaw(Member.Member3M[T1, T2, T3], unionS3_2)
    checkMemberLaw(Member.Member3M[T1, T2, T3], unionS3_3)
  }

  def memberAppendR = {
    Member.MemberAppendR[T3, Fx2[T1, T2], Fx2[T3, T4], Fx1[T4]].project(Union.appendL(Union.twoL(t1))) must beLeft(Union.appendL[Fx2[T1, T2], Fx1[T4], Int](Union.twoL(t1)))
    Member.MemberAppendR[T3, Fx2[T1, T2], Fx2[T3, T4], Fx1[T4]].project(Union.appendL(Union.twoR(t2))) must beLeft(Union.appendL[Fx2[T1, T2], Fx1[T4], Int](Union.twoR(t2)))
    Member.MemberAppendR[T3, Fx2[T1, T2], Fx2[T3, T4], Fx1[T4]].project(Union.appendR(Union.twoL(t3))) must beRight(t3)
    Member.MemberAppendR[T3, Fx2[T1, T2], Fx2[T3, T4], Fx1[T4]].project(Union.appendR(Union.twoR(t4))) must beLeft(Union.appendR[Fx2[T1, T2], Fx1[T4], Int](Union.one(t4)))

    checkMemberLaw(Member.MemberAppendR[T2, Fx1[T1], Fx3[T2, T3, T4], Fx2[T3, T4]], unionS4_1)
    checkMemberLaw(Member.MemberAppendR[T2, Fx1[T1], Fx3[T2, T3, T4], Fx2[T3, T4]], unionS4_2)
    checkMemberLaw(Member.MemberAppendR[T2, Fx1[T1], Fx3[T2, T3, T4], Fx2[T3, T4]], unionS4_3)
    checkMemberLaw(Member.MemberAppendR[T2, Fx1[T1], Fx3[T2, T3, T4], Fx2[T3, T4]], unionS4_4)
  }

  def member3R = {
    Member.Member3R[T1, T2, T3].project(unionS3_1) must beLeft(Union.twoL[T1, T2, Int](t1))
    Member.Member3R[T1, T2, T3].project(unionS3_2) must beLeft(Union.twoR[T1, T2, Int](t2))
    Member.Member3R[T1, T2, T3].project(unionS3_3) must beRight(t3)

    checkMemberLaw(Member.Member3R[T1, T2, T3], unionS3_1)
    checkMemberLaw(Member.Member3R[T1, T2, T3], unionS3_2)
    checkMemberLaw(Member.Member3R[T1, T2, T3], unionS3_3)
  }

  /**
   * HELPERS
   */

  def checkMemberLaw[T[_], R, U, A](m: Member.Aux[T, R, U], union: Union[R, A]) =
    m.project(union).fold(m.accept, m.inject) ==== union

  def checkMemberInOutLaw[T[_], R, A](m: MemberInOut[T, R], ta: T[A]) =
    m.extract(m.inject(ta)) ==== Option(ta)

  val t1 = T1(1)
  val t2 = T2(1)
  val t3 = T3(1)
  val t4 = T4(1)

  val union1 = Union.one(t1)

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
