package org.atnos.site

import org.atnos.eff._
import Eff._
import Effects._
import EvalEffect._
import WriterEffect._
import snippets._, HadoopS3Snippet._
import HadoopStack._
import S3Stack.{WriterString=>_,_}
import cats.syntax.all._

object TransformStack extends UserGuidePage { def is = "Transforming stacks".title ^ s2"""

### Transform an effect to another

Once you get a `Eff[R, A]` action you might want to act on one of the effects, for example to transform `Option` effects
into `Disjunction` effects:${snippet{
import OptionEffect._
import DisjunctionEffect.runDisjunction
import syntax.eff._
import cats.data._
import cats.arrow._

type XorString[A] = String Xor A

type S = Option |: XorString |: NoEffect

implicit val OptionMember =
  Member.aux[Option, S, XorString |: NoEffect]

implicit val XorStringMember =
  Member.aux[XorString, S, Option |: NoEffect]

val map: Map[String, Int] =
  Map("key1" -> 10, "key2" -> 20)

// get 2 keys from the map and add the corresponding values
def addKeys(key1: String, key2: String): Eff[S, Int] = for {
  a <- option(map.get(key1))
  b <- option(map.get(key2))
} yield a + b

// provide a default error message
def addKeysWithDefaultMessage(key1: String, key2: String, message: String): Eff[S, Int] =
  addKeys(key1, key2).transform[Option, XorString](new NaturalTransformation[Option, XorString] {
    def apply[A](o: Option[A]) = o.fold(Xor.left[String, A](message))(Xor.right[String, A])
  })

import DisjunctionImplicits._

(run(runDisjunction(runOption(addKeys("key1", "missing")))),
 run(runDisjunction(runOption(addKeysWithDefaultMessage("key1", "missing", "Key not found")))))

}.eval}
                                                                                        
### Merge stacks

We have seen, in the ${"open-closed" ~/ OpenClosed} section, that we can create effects for a given effect stack, for example
 to interact with a [Hadoop](https://hadoop.apache.org) cluster. We can also define another stack, for storing and retrieving data on [S3](https://aws.amazon.com/s3).
${definition[HadoopS3Snippet]}

So what happens when you want to both use S3 and Hadoop? As you can see from the definition above those 2 stacks share
some common effects, so the resulting stack we want to work with is:${snippet{
import HadoopStack._
import S3Stack.{WriterString=>_,_}

type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect
}}

Then we can use the `into` method to inject effects from each stack into this common stack:${snippet{

// this imports the `into` syntax
import org.atnos.eff.syntax.eff._

val action = for {
  // read a file from hadoop
  s <- readFile("/tmp/data").into[HadoopS3]

  // write a file on S3
  _ <- writeFile("key", s)  .into[HadoopS3]
} yield ()

// and we can run the composite action
run(runEval(runWriter(runHadoopReader(HadoopConf(10))(runS3Reader(S3Conf("bucket"))(action)))))
}.eval}

You can find a fully working example of this approach in `src/test/org/atnos/example/StacksSpec`.
"""

}
