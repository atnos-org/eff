// 8<---
package org.atnos.site.snippets

import HadoopS3Snippet._
import HadoopStack._
import S3Stack.{WriterString=>_,_}

trait HadoopS3Snippet {
// 8<---

import org.atnos.eff._, all._
import cats.data._
import cats.Eval

object HadoopStack {

  case class HadoopConf(mappers: Int)

  type HadoopReader[A] = Reader[HadoopConf, A]
  type WriterString[A] = Writer[String, A]
  type Hadoop = HadoopReader |: WriterString |: Eval |: NoEffect

  def readFile(path: String): Eff[Hadoop, String] =
    for {
      c <- ask[Hadoop, HadoopConf]
      _ <- tell[Hadoop, String]("Reading from "+path)
    } yield c.mappers.toString

  def runHadoopReader[R, U, A](conf: HadoopConf)(e: Eff[R, A])(implicit r: Member.Aux[HadoopReader, R, U]): Eff[U, A] =
    ReaderEffect.runReader(conf)(e)

}

object S3Stack {

  case class S3Conf(bucket: String)

  type S3Reader[A] = Reader[S3Conf, A]
  type WriterString[A] = Writer[String, A]

  type S3 = S3Reader |: WriterString |: Eval |: NoEffect

  def writeFile(key: String, content: String): Eff[S3, Unit] =
    for {
      c <- ask[S3, S3Conf]
      _ <- tell[S3, String]("Writing to bucket "+c.bucket+": "+content)
    } yield ()

  def runS3Reader[R, U, A](conf: S3Conf)(e: Eff[R, A])(implicit r: Member.Aux[S3Reader, R, U]): Eff[U, A] =
    ReaderEffect.runReader(conf)(e)
}

// 8<---

  type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect


}

object HadoopS3Snippet extends HadoopS3Snippet