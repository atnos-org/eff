package org.atnos.eff

import cats.Eval
import cats.data._
import org.specs2.{ScalaCheck, Specification}
import org.atnos.eff.all._
import org.scalacheck._

class MemberSpec extends Specification with ScalaCheck { def is = s2"""

 inject / project must work at the value level
   for reader $reader
   for writer $writer
   for eval   $eval

 project fold (accept, inject) === identity $law

"""
  type WriterString[A] = Writer[String, A]
  type ReaderInt[A] = Reader[Int, A]

  type S = WriterString |: ReaderInt |: Eval |: NoEffect

  implicit def writerMember =
    Member.aux[WriterString, S, ReaderInt |: Eval |: NoEffect]

  implicit def readerMember =
    Member.aux[ReaderInt, S, WriterString |: Eval |: NoEffect]

  implicit def evalMember =
    Member.aux[Eval, S, WriterString |: ReaderInt |: NoEffect]

  val read1 = Reader((i: Int) => "hey")
  val write1 = Writer[String, String]("hey", "hey")
  val eval1 = Eval.later("hey")

  def reader =
    readerMember.project(readerMember.inject(read1)).toEither must beRight(read1)

  def writer =
    writerMember.project(writerMember.inject(write1)).toEither must beRight(write1)

  def eval =
    evalMember.project(evalMember.inject(eval1)).toEither must beRight(eval1)

  trait SMember {
    type T[_]
    val member: Member[T, S]

  }

  def law = Prop.forAll(genUnion, genMember) { (union: Union[S, String], m: SMember) =>
    m.member.project(union).fold(m.member.accept, m.member.inject) ==== union
  }

  def genUnion: Gen[Union[S, String]] =
    Gen.oneOf(
      writerMember.inject(write1),
      evalMember.inject(eval1),
      readerMember.inject(read1))

  def genMember[T[_]]: Gen[SMember] =
    Gen.oneOf(
      new SMember { type T[A] = WriterString[A]; val member = writerMember } ,
      new SMember { type T[A] = Eval[A]; val member = evalMember } ,
      new SMember { type T[A] = ReaderInt[A]; val member = readerMember })

}
