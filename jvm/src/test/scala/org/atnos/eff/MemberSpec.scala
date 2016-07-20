package org.atnos.eff

import cats.Eval
import cats.data._
import cats.implicits._
import org.specs2.{ScalaCheck, Specification}
import org.atnos.eff.all._
import org.atnos.eff.member._
import interpret._
import syntax.all._
import org.scalacheck._
import org.specs2.concurrent.ExecutionEnv

import scala.concurrent.Future

class MemberSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck { def is = s2"""

 inject / project must work at the value level
   for reader $reader
   for writer $writer
   for eval   $eval

 project fold (accept, inject) === identity $law

 it is possible to inject an effect which is part of the remaining stack of a member effect $outMember1
   with a different arrangement                                                             $outMember2
   with a another arrangement                                                               $outMember3
   if the effect is the same as the member effect                                           $outMember4

"""

  def reader =
    readerMember.project(readerMember.inject(read1)).toEither must beRight(read1)

  def writer =
    writerMember.project(writerMember.inject(write1)).toEither must beRight(write1)

  def eval =
    evalMember.project(evalMember.inject(eval1)).toEither must beRight(eval1)

   def outMember1 = {

    type S = Future |: Eval |: Option |: NoEffect

     def run[R :_eval, U](e: Eff[R, Int])(implicit m: Member.Aux[Future, R, U]): Eff[U, Int] = {
      translate(e) { new Translate[Future, U] {
        def apply[X](fx: Future[X]): Eff[U, X] =
          delay(fx.value.get.get)
      }}
    }

    run(pure[S, Int](1) >>= (i => option.some(i * 2))).runEval.runOption.run ==== Option(2)
  }

  def outMember2 = {

    type S = Future |: Eval |: Option |: NoEffect

    def run[R :_option, U](e: Eff[R, Int])(implicit m: Member.Aux[Future, R, U]): Eff[U, Int] = {
      translate(e) { new Translate[Future, U] {
        def apply[X](fx: Future[X]): Eff[U, X] =
          option.some(fx.value.get.get)
      }}
    }

    run(pure[S, Int](1) >>= (i => option.some(i * 2))).runEval.runOption.run ==== Option(2)
  }

  def outMember3 = {

    type S = Option |: Future |: Eval |: NoEffect

    def run[R :_option, U](e: Eff[R, Int])(implicit m: Member.Aux[Future, R, U]): Eff[U, Int] = {
      translate(e) { new Translate[Future, U] {
        def apply[X](fx: Future[X]): Eff[U, X] =
          option.some(fx.value.get.get)
      }}
    }

    run(pure[S, Int](1) >>= (i => option.some(i * 2))).runEval.runOption.run ==== Option(2)
  }

  def outMember4 = {

    type S = Option |: Option |: Future |: Eval |: NoEffect

    def run[R :_option, U](e: Eff[R, Int])(implicit m: Member.Aux[Option, R, U]): Eff[U, Int] = {
      translate(e) { new Translate[Option, U] {
        def apply[X](fx: Option[X]): Eff[U, X] =
          send(fx)
      }}
    }

    run(pure[S, Int](1) >>= (i => option.some(i * 2))).runEval.runOption.detach.value.get.get ==== Option(2)
  }

  /**
   * HELPERS
   */
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
