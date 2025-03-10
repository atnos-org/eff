package org.atnos.eff

import cats.Eval
import cats.arrow.FunctionK
import cats.data.*
import cats.~>
import org.atnos.eff.option.*
import org.atnos.eff.reader.*
import org.atnos.eff.syntax.all.given
import org.scalacheck.*
import org.specs2.*

class MemberSpec extends Specification with ScalaCheck {
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
    Member.MemberAppendL(using writerMember3)

  def readerMember4: Member.Aux[ReaderInt, FxAppend[Fx3[WriterString, ReaderInt, Eval], NoFx], FxAppend[Fx2[WriterString, Eval], NoFx]] =
    Member.MemberAppendL(using readerMember3)

  def evalMember4: Member.Aux[Eval, FxAppend[S3, NoFx], FxAppend[Fx2[WriterString, ReaderInt], NoFx]] =
    Member.MemberAppendL(using evalMember3)

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
      new SMember3 { type T[A] = WriterString[A]; val member: Member[T, S3] = writerMember3 },
      new SMember3 { type T[A] = Eval[A]; val member: Member[T, S3] = evalMember3 },
      new SMember3 { type T[A] = ReaderInt[A]; val member: Member[T, S3] = readerMember3 }
    )

  def genUnion4: Gen[Union[S4, String]] =
    Gen.oneOf(writerMember4.inject(write1), evalMember4.inject(eval1), readerMember4.inject(read1))

  def genMember4[T[_]]: Gen[SMember4] =
    Gen.oneOf(
      new SMember4 { type T[A] = WriterString[A]; val member: Member[T, S4] = writerMember4 },
      new SMember4 { type T[A] = Eval[A]; val member: Member[T, S4] = evalMember4 },
      new SMember4 { type T[A] = ReaderInt[A]; val member: Member[T, S4] = readerMember4 }
    )

  type readStr[E] = Reader[String, *] |= E
  type stateStr[E] = State[String, *] |= E

  type ReadStr[E] = Reader[String, *] /= E
  type StateStr[E] = State[String, *] /= E

  given readerStateNat[S1]: FunctionK[Reader[S1, *], State[S1, *]] = new (Reader[S1, *] ~> State[S1, *]) {
    def apply[X](r: Reader[S1, X]): State[S1, X] =
      State((s: S1) => (s, r.run(s)))
  }

  given stateReaderNat[S1]: FunctionK[State[S1, *], Reader[S1, *]] = new (State[S1, *] ~> Reader[S1, *]) {
    def apply[X](state: State[S1, X]): Reader[S1, X] =
      Reader((s: S1) => state.runA(s).value)
  }

  def readIn[E: readStr: _option]: Eff[E, Int] =
    for {
      s <- ask[E, String]
      _ <- option.some(s)
    } yield s.size

  def stateIn[E](using state: State[String, *] |= E, option: Option |= E): Eff[E, Int] =
    readIn[E](using state.transform[Reader[String, *]], option)

  def readInOut[E: ReadStr: _option]: Eff[E, Int] =
    for {
      s <- ask[E, String]
      _ <- option.some(s)
    } yield s.size

  def stateInOut[E](using state: State[String, *] /= E, option: Option |= E): Eff[E, Int] =
    readInOut[E](using state.transform[Reader[String, *]], option)
}
