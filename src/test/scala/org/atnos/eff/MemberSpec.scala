package org.atnos.eff

import cats.Eval
import cats.data._
import org.specs2.Specification
import org.atnos.eff.all._
import org.atnos.eff.implicits._

class MemberSpec extends Specification { def is = s2"""

 inject / project must work at the value level
   for reader $reader
   for writer $writer
   for eval   $eval

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

  def reader = {
    val read1 = Reader((i: Int) => "hey")
    readerMember.project(readerMember.inject(read1)).toEither must beRight(read1)
  }

  def writer = {
    val write1 = Writer("hey", ())
    writerMember.project(writerMember.inject(write1)).toEither must beRight(write1)
  }

  def eval = {
    val eval1 = Eval.later("hey")
    evalMember.project(evalMember.inject(eval1)).toEither must beRight(eval1)
  }

}
