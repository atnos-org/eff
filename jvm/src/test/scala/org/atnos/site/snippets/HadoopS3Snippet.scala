// 8<---
package org.atnos.site.snippets

import HadoopS3Snippet._
import HadoopStack._
import S3Stack.{WriterString=>_,_}

trait HadoopS3Snippet {
// 8<---

import org.atnos.eff._, all._
import cats.data._

object HadoopStack {

  case class HadoopConf(mappers: Int)

  type HadoopReader[A] = Reader[HadoopConf, A]
  type WriterString[A] = Writer[String, A]
  type Hadoop = HadoopReader |: WriterString |: Eval |: NoEffect

  object Hadoop {
    implicit val HadoopReaderMember: Member.Aux[HadoopReader, Hadoop, WriterString |: Eval |: NoEffect] =
      Member.first

    implicit val WriterStringMember: Member.Aux[WriterString, Hadoop, HadoopReader |: Eval |: NoEffect] =
      Member.successor

    implicit val EvalMember: Member.Aux[Eval, Hadoop, HadoopReader |: WriterString |: NoEffect] =
      Member.successor
  }

  import Hadoop._

  def readFile(path: String): Eff[Hadoop, String] =
    for {
      c <- ask[Hadoop, HadoopConf]
      _ <- tell("Reading from "+path)
    } yield c.mappers.toString

  def runHadoopReader[R, A](conf: HadoopConf): Eff[HadoopReader |: R, A] => Eff[R, A] =
    (e: Eff[HadoopReader |: R, A]) => ReaderEffect.runReader(conf)(e)

}

object S3Stack {

  case class S3Conf(bucket: String)

  type S3Reader[A] = Reader[S3Conf, A]
  type WriterString[A] = Writer[String, A]

  type S3 = S3Reader |: WriterString |: Eval |: NoEffect

  object S3 {
    implicit val S3ReaderMember: Member.Aux[S3Reader, S3, WriterString |: Eval |: NoEffect] =
      Member.first

    implicit val WriterStringMember: Member.Aux[WriterString, S3, S3Reader |: Eval |: NoEffect] =
      Member.successor

    implicit val EvalMember: Member.Aux[Eval, S3, S3Reader |: WriterString |: NoEffect] =
      Member.successor
  }

  import S3._


  def writeFile(key: String, content: String): Eff[S3, Unit] =
    for {
      c <- ask[S3, S3Conf]
      _ <- tell("Writing to bucket "+c.bucket+": "+content)
    } yield ()

  def runS3Reader[R, A](conf: S3Conf): Eff[S3Reader |: R, A] => Eff[R, A] =
    (e: Eff[S3Reader |: R, A]) => ReaderEffect.runReader(conf)(e)
}

// 8<---

  type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect

  object HadoopS3 {
    implicit val S3ReaderMember: Member.Aux[S3Reader, HadoopS3, HadoopReader |: WriterString |: Eval |: NoEffect] =
      Member.first

    implicit val HadoopReaderMember: Member.Aux[HadoopReader, HadoopS3, S3Reader |: WriterString |: Eval |: NoEffect] =
      Member.successor

    implicit val WriterStringMember: Member.Aux[WriterString, HadoopS3, S3Reader |: HadoopReader |: Eval |: NoEffect] =
      Member.successor

    implicit val EvalMember: Member.Aux[Eval, HadoopS3, S3Reader |: HadoopReader |: WriterString |: NoEffect] =
      Member.successor

  }

}

object HadoopS3Snippet extends HadoopS3Snippet

