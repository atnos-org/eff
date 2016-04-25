package org.atnos.site

import snippets._, HadoopS3Snippet._
import HadoopStack._
import S3Stack.{WriterString=>_,_}
import cats.syntax.all._

object TransformStack extends UserGuidePage { def is = "Transforming stacks".title ^ s2"""

### Transform an effect to another

#### Change the effect

A typical use case for this is to transform a stack having a `Reader[S, ?]` effect to a stack having a `Reader[B, ?]` effect
 where `S` is "contained" in `B` (meaning that there is a mapping from `B`, "big", to `S`, "small"). Here is an example:${snippet{
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import cats.data._

case class Conf(host: String, port: Int)

type ReaderPort[A] = Reader[Int, A]
type ReaderHost[A] = Reader[String, A]
type ReaderConf[A] = Reader[Conf, A]

type S1 = ReaderHost |: Option |: NoEffect
type S2 = ReaderPort |: Option |: NoEffect
type SS = ReaderConf |: Option |: NoEffect

implicit val ReaderHostMember: Member.Aux[ReaderHost, S1, Option |: NoEffect] =
  Member.first

implicit val ReaderPortMember: Member.Aux[ReaderPort, S2, Option |: NoEffect] =
  Member.first

implicit val ReaderConfMember: Member.Aux[ReaderConf, SS, Option |: NoEffect] =
  Member.first

val readHost: Eff[S1, String] = for {
  c <- ask[S1, String]
  h <- OptionEffect.some[S1, String]("hello")
} yield h

val readPort: Eff[S2, String] = for {
  c <- ask[S2, Int]
  h <- OptionEffect.some[S2, String]("world")
} yield h

val action: Eff[SS, String] = for {
  s1 <- localReader(readHost, (c: Conf) => c.host)
  s2 <- localReader(readPort, (c: Conf) => c.port)
} yield s1 + " " + s2

action.runReader(Conf("www.me.com", 8080)).runOption.run

}.eval}

More generally the method `Interpret.transform(eff, naturalTransformation)` provides a way to interpret one effect to
another using a natural transformation (changing just one effect in the stack).

#### From one effect to another in the same stack

Once you get a `Eff[R, A]` action you might want to act on one of the effects, for example to swap the `Option` effect
with the `Either` effect if both are present in the stack:${snippet{
import org.atnos.eff._, all._
import org.atnos.eff.syntax.all._
import cats.data._
import cats.arrow._

object S {
  type XorString[A] = String Xor A

  type S = Option |: XorString |: NoEffect

  implicit val OptionMember: Member.Aux[Option, S, XorString |: NoEffect] =
    Member.first

  implicit val XorStringMember: Member.Aux[XorString, S, Option |: NoEffect] =
    Member.successor
}
import S._

val map: Map[String, Int] =
  Map("key1" -> 10, "key2" -> 20)

// get 2 keys from the map and add the corresponding values
def addKeys(key1: String, key2: String): Eff[S, Int] = for {
  a <- option(map.get(key1))
  b <- option(map.get(key2))
} yield a + b

// describe how options are transformed into eithers
def nat(message: String) = new NaturalTransformation[Option, XorString] {
  def apply[A](o: Option[A]) = o.fold(Xor.left[String, A](message))(Xor.right[String, A])
}

// provide a default error message
def addKeysWithDefaultMessage(key1: String, key2: String, message: String): Eff[S, Int] =
  addKeys(key1, key2).swap[Option, XorString](nat(message))

import org.atnos.eff.implicits._

(addKeys("key1", "missing").runOption.runXor.run,
 addKeysWithDefaultMessage("key1", "missing", "Key not found").runOption.runXor.run)

}.eval}

Note that the stack stays the same before and after, even if the original effect is not used anymore.

### Merge stacks

We have seen, in the ${"open-closed" ~/ OpenClosed} section, that we can create effects for a given effect stack, for example
 to interact with a [Hadoop](https://hadoop.apache.org) cluster. We can also define another stack, for storing and retrieving data on [S3](https://aws.amazon.com/s3).
${definition[HadoopS3Snippet]}

So what happens when you want to both use S3 and Hadoop? As you can see from the definition above those 2 stacks share
some common effects, so the resulting stack we want to work with is:${snippet{
import org.atnos.eff._, Effects._
import cats.Eval
import HadoopStack._
import S3Stack.{WriterString=>_,_}

type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect
}}

Then we can use the `into` method to inject effects from each stack into this common stack:${snippet{

// this imports the `into` and runXXX syntax
import org.atnos.eff.syntax.all._

val action = for {
  // read a file from hadoop
  s <- readFile("/tmp/data").into[HadoopS3]

  // write a file on S3
  _ <- writeFile("key", s)  .into[HadoopS3]
} yield ()

import HadoopS3._
import org.atnos.eff.implicits._

// and we can run the composite action
action.runReaderTagged(S3Conf("bucket")).runReaderTagged(HadoopConf(10)).runWriter.runEval.run
}.eval}

You can find a fully working example of this approach in `src/test/org/atnos/example/StacksSpec`.
"""

}
