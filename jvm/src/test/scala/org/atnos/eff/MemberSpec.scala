package org.atnos.eff

import cats.{Eval, ~>}
import cats.data._
import cats.implicits._
import org.specs2._
import org.scalacheck._
import option._
import reader._
import state._
import syntax.all._

class MemberSpec extends Specification with ScalaCheck { def is = s2"""

 inject / project must work at the value level
   for reader $reader
   for writer $writer
   for eval   $eval

 extract . inject === Option.apply $lawMemberIn
 project fold (accept, inject) === identity $lawMember

 A MemberIn instance can be transformed with natural transformations    $natIn
 A MemberInOut instance can be transformed with natural transformations $natInOut

"""

  def reader =
    readerMember.project(readerMember.inject(read1)).toEither must beRight(read1)

  def writer =
    writerMember.project(writerMember.inject(write1)).toEither must beRight(write1)

  def eval =
    evalMember.project(evalMember.inject(eval1)).toEither must beRight(eval1)

  def lawMemberIn =
    writerMember.extract(writerMember.inject(write1)) ==== Option(write1)

  def lawMember = Prop.forAll(genUnion, genMember) { (union: Union[S, String], m: SMember) =>
    m.member.project(union).fold(m.member.accept, m.member.inject) ==== union
  }

  def natIn =
    stateIn[Fx2[State[String, ?], Option]].runOption.evalState("hello").run ==== Option(5)

  def natInOut =
    stateInOut[Fx2[State[String, ?], Option]].runOption.evalState("hello").run ==== Option(5)

  /**
   * HELPERS
   */
  type WriterString[A] = Writer[String, A]
  type ReaderInt[A] = Reader[Int, A]

  type S = Fx3[WriterString, ReaderInt, Eval]

  def writerMember =
    Member.Member3L[WriterString, ReaderInt, Eval]

  def readerMember =
    Member.Member3M[WriterString, ReaderInt, Eval]

  def evalMember: Member.Aux[Eval, S, Fx2[WriterString, ReaderInt]] =
    Member.Member3R[WriterString, ReaderInt, Eval]

  val read1  = Reader((i: Int) => "hey")
  val write1 = Writer[String, String]("hey", "hey")
  val eval1  = Eval.later("hey")

  trait SMember {
    type T[_]
    val member: Member[T, S]

  }

  def genUnion: Gen[Union[S, String]] =
    Gen.oneOf(
      writerMember.inject(write1),
      evalMember.inject(eval1),
      readerMember.inject(read1))

  def genMember[T[_]]: Gen[SMember] =
    Gen.oneOf(
      new SMember { type T[A] = WriterString[A]; val member = writerMember } ,
      new SMember { type T[A] = Eval[A];         val member = evalMember } ,
      new SMember { type T[A] = ReaderInt[A];    val member = readerMember })

  type readStr[E] = Reader[String, ?] |= E
  type stateStr[E] = State[String, ?] |= E

  type ReadStr[E] = Reader[String, ?] /= E
  type RtateStr[E] = State[String, ?] /= E

  implicit def readerStateNat[S1] = new (Reader[S1, ?] ~> State[S1, ?]) {
    def apply[X](r: Reader[S1, X]): State[S1, X] =
      State((s: S1) => (s, r.run(s)))
  }

  implicit def stateReaderNat[S1] = new (State[S1, ?] ~> Reader[S1, ?]) {
    def apply[X](state: State[S1, X]): Reader[S1, X] =
      Reader((s: S1) => state.runA(s).value)
  }

  def readIn[E: readStr: _option]: Eff[E, Int] =
    for {
      s <- ask[E, String]
      _ <- option.some(s)
    } yield s.size

  def stateIn[E](implicit state: State[String, ?] |= E, option: Option |= E): Eff[E, Int] =
    readIn[E](state.transform[Reader[String, ?]], option)

  def readInOut[E: ReadStr: _option]: Eff[E, Int] =
    for {
      s <- ask[E, String]
      _ <- option.some(s)
    } yield s.size

  def stateInOut[E](implicit state: State[String, ?] /= E, option: Option |= E): Eff[E, Int] =
    readIn[E](state.transform[Reader[String, ?]], option)
}
