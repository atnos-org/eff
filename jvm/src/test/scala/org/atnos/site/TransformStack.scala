package org.atnos.site

import snippets._
import HadoopS3Snippet._
import HadoopStack._
import S3Stack.{WriterString => _, _}
import cats.~>

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

val readHost: Eff[S1, String] = for {
  c <- ask[S1, String]
  h <- OptionEffect.some[S1, String]("hello")
} yield h

val readPort: Eff[S2, String] = for {
  c <- ask[S2, Int]
  h <- OptionEffect.some[S2, String]("world")
} yield h

val fromHost = new (ReaderHost ~> ReaderConf) {
  def apply[X](r: ReaderHost[X]) = Reader((c: Conf) => r.run(c.host))
}

val fromPort = new (ReaderPort ~> ReaderConf) {
  def apply[X](r: ReaderPort[X]) = Reader((c: Conf) => r.run(c.port))
}

val action: Eff[SS, String] = for {
  s1 <- readHost.transform(fromHost)
  s2 <- readPort.transform(fromPort)
} yield s1 + " " + s2

action.runReader(Conf("www.me.com", 8080)).runOption.run
}.eval}

There are also specialized versions of `transform` for `Reader` and `State`:

 - `ReaderEffect.localReader` takes a "getter" `B => A` to transform a stack with a `Reader[A, ?]` into a stack with a `Reader[B, ?]`
 - `StateEffect.lensState` takes a "getter" `S => T` and a "setter" `(S, T) => S` to to transform a stack with a `State[T, ?]` into a stack with a `State[S, ?]`

### Merge stacks

We can create effects for a given effect stack, for example to interact with a [Hadoop](https://hadoop.apache.org) cluster.
 We can also define another stack, for storing and retrieving data on [S3](https://aws.amazon.com/s3).
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

// and we can run the composite action
action.runReader(S3Conf("bucket")).runReader(HadoopConf(10)).runWriter.runEval.run
}.eval}

You can find a fully working example of this approach in `src/test/org/atnos/example/StacksSpec`.
"""

}
