package org.atnos
package example

import org.specs2._
import cats.data._
import cats.Eval
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

class StacksSpec extends Specification { def is = s2"""

 Some domains may use different effect stacks sharing some common effects.
 When we want to use functions from 2 different domains we want to unify all the effects into one stack.

 There are 2 ways to do this:

  1. each domain can define functions only requiring effects to be members of a given yet-undefined stack R

    In that case it will be straightforward to combine functions from both domains.

  2. each domain can define functions for a given stack of effects

    In that case we need to use the `into` method to adapt functions returning `Eff[Stack, A]` into `Eff[SuperStack, A]`
    where `SuperStack` contains all the effects of `Stack`.

  The following examples use a Hadoop domain for functions interacting with Hadoop and a S3 domain for functions interacting
  with S3. Notably, the Hadoop stack contains a Reader effect to access the Hadoop configuration whereas the
  S3 stack contains another Reader effect to access the S3 configuration. If we want to interact with both Hadoop and S3,
  we define a HadoopS3 stack containing all the effects from the 2 stacks.

  Two stacks defined with open effects can be used together           $togetherOpen
  Two stacks using different effects can be used together with `into` $together

"""

  def togetherOpen = {

    import HadoopOpenStack._
    import S3OpenStack.{WriterString=>_,_}

    type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect

    val action = for {
      s <- readFile[HadoopS3]("/tmp/data")
      _ <- writeFile[HadoopS3]("key", s)
    } yield ()

    runHadoopReader(HadoopConf(10))(runS3Reader(S3Conf("bucket"))(action.fx)).runWriter.runEval.run ====
      (((), List("Reading from /tmp/data", "Writing to bucket bucket: 10")))
  }

  def together = {
    import HadoopClosedStack._
    import S3ClosedStack.{WriterString=>_,_}

    type HadoopS3 = S3Reader |: HadoopReader |: WriterString |:: Eval

    val action = for {
      s <- readFile("/tmp/data").into[HadoopS3]
      _ <- writeFile("key", s)  .into[HadoopS3]
    } yield ()

    run(runEval(runWriter(runHadoopReader(HadoopConf(10))(runS3Reader(S3Conf("bucket"))(action.fx))))) ====
      (((), List("Reading from /tmp/data", "Writing to bucket bucket: 10")))
  }

  /**
   * STACKS
   */

  object HadoopOpenStack {

    case class HadoopConf(mappers: Int)

    type HadoopReader[A] = Reader[HadoopConf, A]
    type WriterString[A] = Writer[String, A]
    type Hadoop = HadoopReader |: WriterString |: Eval |: NoEffect

    def readFile[R](path: String)(implicit r: HadoopReader |= R, w: WriterString |= R): Eff[R, String] =
      for {
        c <- ask[R, HadoopConf](r)
        _ <- tell[R, String]("Reading from "+path)(w)
      } yield c.mappers.toString

    def runHadoopReader[R, U, A](conf: HadoopConf)(e: Eff[R, A])(implicit m: Member.Aux[HadoopReader, R, U]): Eff[U, A] =
      ReaderEffect.runReader(conf)(e)

  }

  object S3OpenStack {

    case class S3Conf(bucket: String)

    type S3Reader[A] = Reader[S3Conf, A]
    type WriterString[A] = Writer[String, A]

    type S3 = S3Reader |: WriterString |: Eval |: NoEffect

    def writeFile[R](key: String, content: String)(implicit r: S3Reader |= R, w: WriterString |= R): Eff[R, Unit] =
      for {
        c <- ask[R, S3Conf](r)
        _ <- tell[R, String]("Writing to bucket "+c.bucket+": "+content)(w)
      } yield ()

    def runS3Reader[R, U, A](conf: S3Conf)(e: Eff[R, A])(implicit m: Member.Aux[S3Reader, R, U]): Eff[U, A] =
      ReaderEffect.runReader(conf)(e)
  }

  object HadoopClosedStack {

    case class HadoopConf(mappers: Int)

    type HadoopReader[A] = Reader[HadoopConf, A]
    type WriterString[A] = Writer[String, A]
    type Hadoop = HadoopReader |: WriterString |: Eval |: NoEffect

    def readFile(path: String): Eff[Hadoop, String] =
      for {
        c <- ask[Hadoop, HadoopConf]
        _ <- tell[Hadoop, String]("Reading from "+path)
      } yield c.mappers.toString

    def runHadoopReader[R, U, A](conf: HadoopConf)(e: Eff[R, A])(implicit m: Member.Aux[HadoopReader, R, U]): Eff[U, A] =
      ReaderEffect.runReader(conf)(e)

  }

  object S3ClosedStack {

    case class S3Conf(bucket: String)

    type S3Reader[A] = Reader[S3Conf, A]
    type WriterString[A] = Writer[String, A]

    type S3 = S3Reader |: WriterString |: Eval |: NoEffect

    def writeFile(key: String, content: String): Eff[S3, Unit] =
      for {
        c <- ask[S3, S3Conf]
        _ <- tell[S3, String]("Writing to bucket "+c.bucket+": "+content)
      } yield ()

    def runS3Reader[R, U, A](conf: S3Conf)(e: Eff[R, A])(implicit m: Member.Aux[S3Reader, R, U]): Eff[U, A] =
      ReaderEffect.runReader(conf)(e)
  }
}
