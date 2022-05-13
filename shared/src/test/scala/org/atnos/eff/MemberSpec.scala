package org.atnos.eff

import cats.Eval
import cats.~>
import cats.data._
import org.specs2._
import org.scalacheck._
import option._
import reader._
import state._
import syntax.all._

class MemberSpec extends Specification with ScalaCheck with Specs2Compat {
  def is = s2"""

for 3-element stacks:

   inject / project must work at the value level
     for reader $reader3
     for writer $writer3
     for eval   $eval3

   extract . inject === Option.apply $lawMemberIn3
   project fold (accept, inject) === identity $lawMember3

for 4-element stacks:

   inject / project must work at the value level
     for reader $reader4
     for writer $writer4
     for eval   $eval4

   extract . inject === Option.apply $lawMemberIn4
   project fold (accept, inject) === identity $lawMember4

 A MemberIn instance can be transformed with natural transformations    $natIn
 A MemberInOut instance can be transformed with natural transformations $natInOut

"""

  def reader3 =
    readerMember3.project(readerMember3.inject(read1)) must beRight(read1)

  def writer3 =
    writerMember3.project(writerMember3.inject(write1)) must beRight(write1)

  def eval3 =
    evalMember3.project(evalMember3.inject(eval1)) must beRight(eval1)

  def lawMemberIn3 =
    writerMember3.extract(writerMember3.inject(write1)) ==== Option(write1)

  def lawMember3 = Prop.forAll(genUnion3, genMember3) { (union: Union[S3, String], m: SMember3) =>
    m.member.project(union).fold(m.member.accept, m.member.inject) ==== union
  }

  def reader4 =
    readerMember4.project(readerMember4.inject(read1)) must beRight(read1)

  def writer4 =
    writerMember4.project(writerMember4.inject(write1)) must beRight(write1)

  def eval4 =
    evalMember4.project(evalMember4.inject(eval1)) must beRight(eval1)

  def lawMemberIn4 =
    writerMember4.extract(writerMember4.inject(write1)) ==== Option(write1)

  def lawMember4 = Prop.forAll(genUnion4, genMember4) { (union: Union[S4, String], m: SMember4) =>
    m.member.project(union).fold(m.member.accept, m.member.inject) ==== union
  }

  def natIn =
    stateIn[Fx2[State[String, *], Option]].runOption.evalState("hello").run ==== Option(5)

  def natInOut =
    stateInOut[Fx2[State[String, *], Option]].runOption.evalState("hello").run ==== Option(5)

  /**
   * HELPERS
   */
  type WriterString[A] = Writer[String, A]
  type ReaderInt[A] = Reader[Int, A]

  type S3 = Fx3[WriterString, ReaderInt, Eval]
  type S4 = FxAppend[Fx3[WriterString, ReaderInt, Eval], NoFx]

  def writerMember3 =
    Member.Member3L[WriterString, ReaderInt, Eval]

  def readerMember3 =
    Member.Member3M[WriterString, ReaderInt, Eval]

  def evalMember3: Member.Aux[Eval, S3, Fx2[WriterString, ReaderInt]] =
    Member.Member3R[WriterString, ReaderInt, Eval]

  def writerMember4: Member.Aux[WriterString, FxAppend[Fx3[WriterString, ReaderInt, Eval], NoFx], FxAppend[Fx2[ReaderInt, Eval], NoFx]] =
    Member.MemberAppendL(writerMember3)

  def readerMember4: Member.Aux[ReaderInt, FxAppend[Fx3[WriterString, ReaderInt, Eval], NoFx], FxAppend[Fx2[WriterString, Eval], NoFx]] =
    Member.MemberAppendL(readerMember3)

  def evalMember4: Member.Aux[Eval, FxAppend[S3, NoFx], FxAppend[Fx2[WriterString, ReaderInt], NoFx]] =
    Member.MemberAppendL(evalMember3)

  val read1 = Reader((i: Int) => "hey")
  val write1 = Writer[String, String]("hey", "hey")
  val eval1 = Eval.later("hey")

  trait SMember3 {
    type T[_]
    val member: Member[T, S3]
  }

  trait SMember4 {
    type T[_]
    val member: Member[T, S4]
  }

  def genUnion3: Gen[Union[S3, String]] =
    Gen.oneOf(writerMember3.inject(write1), evalMember3.inject(eval1), readerMember3.inject(read1))

  def genMember3[T[_]]: Gen[SMember3] =
    Gen.oneOf(
      new SMember3 { type T[A] = WriterString[A]; val member = writerMember3 },
      new SMember3 { type T[A] = Eval[A]; val member = evalMember3 },
      new SMember3 { type T[A] = ReaderInt[A]; val member = readerMember3 }
    )

  def genUnion4: Gen[Union[S4, String]] =
    Gen.oneOf(writerMember4.inject(write1), evalMember4.inject(eval1), readerMember4.inject(read1))

  def genMember4[T[_]]: Gen[SMember4] =
    Gen.oneOf(
      new SMember4 { type T[A] = WriterString[A]; val member = writerMember4 },
      new SMember4 { type T[A] = Eval[A]; val member = evalMember4 },
      new SMember4 { type T[A] = ReaderInt[A]; val member = readerMember4 }
    )

  type readStr[E] = Reader[String, *] |= E
  type stateStr[E] = State[String, *] |= E

  type ReadStr[E] = Reader[String, *] /= E
  type StateStr[E] = State[String, *] /= E

  implicit def readerStateNat[S1]: Reader[S1, *] ~> State[S1, *] = new Reader[S1, *] ~> State[S1, *] {
    def apply[X](r: Reader[S1, X]): State[S1, X] =
      State((s: S1) => (s, r.run(s)))
  }

  implicit def stateReaderNat[S1]: State[S1, *] ~> Reader[S1, *] = new State[S1, *] ~> Reader[S1, *] {
    def apply[X](state: State[S1, X]): Reader[S1, X] =
      Reader((s: S1) => state.runA(s).value)
  }

  def readIn[E: readStr: _option]: Eff[E, Int] =
    for {
      s <- ask[E, String]
      _ <- option.some(s)
    } yield s.size

  def stateIn[E](implicit state: State[String, *] |= E, option: Option |= E): Eff[E, Int] =
    readIn[E](state.transform[Reader[String, *]], option)

  def readInOut[E: ReadStr: _option]: Eff[E, Int] =
    for {
      s <- ask[E, String]
      _ <- option.some(s)
    } yield s.size

  def stateInOut[E](implicit state: State[String, *] /= E, option: Option |= E): Eff[E, Int] =
    readInOut[E](state.transform[Reader[String, *]], option)
}
