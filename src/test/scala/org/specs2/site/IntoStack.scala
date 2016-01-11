package org.specs2.site

import org.specs2.control.eff._
import Eff._
import Effects._
import EvalEffect._
import WriterEffect._
import cats.data._
import cats.syntax.all._
import Tag._
import HadoopStack._
import S3Stack.{WriterString=>_,_}

object IntoStack extends UserGuidePage { def is = "Effect stacks".title ^ s2"""

We have seen, in the [open-closed](open-closed.md) section, that we can create effects for a given effect stack, for example
 to interact with a [Hadoop](https://hadoop.apache.org) cluster. We can also define another stack, for storing and retrieving data on [S3](https://aws.amazon.com/s3). ${snippet{
import org.specs2.control.eff._
import Eff._
import Effects._
import EvalEffect._
import WriterEffect._
import cats.data._
import cats.syntax.all._
import Tag._

object HadoopStack {
  trait HadoopTag
  case class HadoopConf(mappers: Int)

  type HadoopReader[A] = Reader[HadoopConf, A] @@ HadoopTag
  type WriterString[A] = Writer[String, A]
  type Hadoop = HadoopReader |: WriterString |: Eval |: NoEffect

  implicit def HadoopReaderMember: Member[HadoopReader, Hadoop] =
    Member.MemberNatIsMember

  implicit def WriterStringMember: Member[WriterString, Hadoop] =
    Member.MemberNatIsMember

  def askHadoopConf: Eff[Hadoop, HadoopConf] =
    ReaderEffect.askTagged

  def readFile(path: String): Eff[Hadoop, String] =
    for {
      c <- askHadoopConf
      _ <- tell("Reading from "+path)
    } yield c.mappers.toString

  def runHadoopReader[R <: Effects, A](conf: HadoopConf): Eff[HadoopReader |: R, A] => Eff[R, A] =
    (e: Eff[HadoopReader |: R, A]) => ReaderEffect.runTaggedReader(conf)(e)

}

object S3Stack {
  trait S3Tag
  case class S3Conf(bucket: String)

  type S3Reader[A] = Reader[S3Conf, A] @@ S3Tag
  type WriterString[A] = Writer[String, A]

  type S3 = S3Reader |: WriterString |: Eval |: NoEffect

  implicit def S3ReaderMember: Member[S3Reader, S3] =
    Member.MemberNatIsMember

  implicit def WriterStringMember: Member[WriterString, S3] =
    Member.MemberNatIsMember

  def askS3Conf: Eff[S3, S3Conf] =
    ReaderEffect.askTagged

  def writeFile(key: String, content: String): Eff[S3, Unit] =
    for {
      c <- askS3Conf
      _ <- tell("Writing to bucket "+c.bucket+": "+content)
    } yield ()

  def runS3Reader[R <: Effects, A](conf: S3Conf): Eff[S3Reader |: R, A] => Eff[R, A] =
    (e: Eff[S3Reader |: R, A]) => ReaderEffect.runTaggedReader(conf)(e)
}
}}

So what happens when you want to both use S3 and Hadoop? As you can see from the definition above those 2 stacks share
some common effects, so the resulting stack we want to work with is:${snippet{
import HadoopStack._
import S3Stack.{WriterString=>_,_}

type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect
}}

Then we can use the `into` method to inject effects from each stack into this common stack:${snippet{

// this imports the `into` syntax
import org.specs2.control.eff.syntax.eff._

val action = for {
  // read a file from hadoop
  s <- readFile("/tmp/data").into[HadoopS3]

  // write a file on S3
  _ <- writeFile("key", s)  .into[HadoopS3]
} yield ()

// and we can run the composite action
run(runEval(runWriter(runHadoopReader(HadoopConf(10))(runS3Reader(S3Conf("bucket"))(action)))))
}.eval}

You can find a fully working example of this approach in `src/test/org/specs2/example/StacksSpec`.
"""

 type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect

}


object HadoopStack {
  trait HadoopTag
  case class HadoopConf(mappers: Int)

  type HadoopReader[A] = Reader[HadoopConf, A] @@ HadoopTag
  type WriterString[A] = Writer[String, A]
  type Hadoop = HadoopReader |: WriterString |: Eval |: NoEffect

  implicit def HadoopReaderMember: Member[HadoopReader, Hadoop] =
    Member.MemberNatIsMember

  implicit def WriterStringMember: Member[WriterString, Hadoop] =
    Member.MemberNatIsMember

  def askHadoopConf: Eff[Hadoop, HadoopConf] =
    ReaderEffect.askTagged

  def readFile(path: String): Eff[Hadoop, String] =
    for {
      c <- askHadoopConf
      _ <- tell("Reading from "+path)
    } yield c.mappers.toString

  def runHadoopReader[R <: Effects, A](conf: HadoopConf): Eff[HadoopReader |: R, A] => Eff[R, A] =
    (e: Eff[HadoopReader |: R, A]) => ReaderEffect.runTaggedReader(conf)(e)

}

object S3Stack {
  trait S3Tag
  case class S3Conf(bucket: String)

  type S3Reader[A] = Reader[S3Conf, A] @@ S3Tag
  type WriterString[A] = Writer[String, A]

  type S3 = S3Reader |: WriterString |: Eval |: NoEffect

  implicit def S3ReaderMember: Member[S3Reader, S3] =
    Member.MemberNatIsMember

  implicit def WriterStringMember: Member[WriterString, S3] =
    Member.MemberNatIsMember

  def askS3Conf: Eff[S3, S3Conf] =
    ReaderEffect.askTagged

  def writeFile(key: String, content: String): Eff[S3, Unit] =
    for {
      c <- askS3Conf
      _ <- tell("Writing to bucket "+c.bucket+": "+content)
    } yield ()

  def runS3Reader[R <: Effects, A](conf: S3Conf): Eff[S3Reader |: R, A] => Eff[R, A] =
    (e: Eff[S3Reader |: R, A]) => ReaderEffect.runTaggedReader(conf)(e)
}

