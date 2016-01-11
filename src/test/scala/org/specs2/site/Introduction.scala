package org.specs2.site

import cats.data._
import org.specs2.control.eff._
import Effects._
import EvalEffect._

object Introduction extends UserGuidePage { def is = "Introduction".title ^ s2"""

Extensible effects are an alternative to monad transformers for computing with effects in a functional way.
This library uses a "free-er" monad and an "open-union" of effects to create an "effect stack" as described in
[Oleg Kiselyov's paper](http://okmij.org/ftp/Haskell/extensible/more.pdf).

There are lots of advantages to this approach:

 - operations can be declared as "requiring" an effect, without the need to fix the full stack of effects in advance

 - effects handlers are modular and can be replaced with other implementations as needed (even at runtime)

 - the underlying implementation is performant and stack-safe

 - existing monad datatypes can be integrated to the library

 - it is possible to integrate different effect stacks into one

This is probably very abstract so let's see more precisely what this all means.

### First example

A monadic action is modelled as a value of type `Eff[R, A]` where `R` denotes a set of effects and `A` is the value
returned by the computation, possibly triggering some effects when evaluated.

The effects `R` are modelled by a type-level list of "effect constructors", for example:${snippet{
import cats.data._
import org.specs2.control.eff._
import Effects._
import EvalEffect._

type ReaderInt[X] = Reader[Int, X]
type WriterString[X] = Writer[String, X]

type Stack = ReaderInt |: WriterString |: Eval |: NoEffect

}}
The stack `Stack` above declares 3 effects:

 - a `ReaderInt` effect to access some configuration number of type `Int`

 - a `WriterString` effect to log string messages

 - an `Eval` effect to only compute values on demand (a bit like lazy values)

Now we can write a program with those 3 effects, using the primitive operations provided by `ReaderEffect`, `WriterEffect` and `EvalEffect`:${snippet{
import Eff._
import cats.syntax.all._
import ReaderCreation._
import WriterCreation._

val program: Eff[Stack, Int] = for {
  // get the configuration
  n <- ask

  // log the current configuration value
  _ <- tell("the required power is "+n)

  // compute the nth power of 2
  a <- delay(math.pow(2, n.toDouble).toInt)

  // log the result
  _ <- tell("the result is "+a)
} yield a

import ReaderEffect._
import WriterEffect._

// run the action with all the interpreters
// each interpreter running one effect
run(runWriter(runEval(runReader(6)(program))))
}.eval}

As you can see, all the effects of the `Stack` type are being executed one by one:

 1. the `Reader` effect, needing a value to inject
 2. the `Writer` effect, which logs values
 3. the `Eval` effect to compute the "power of 2 computation"
 4. finally the `NoEffect` effect (provided by the `Eff` object) to get the final value out of `Eff[Stack, Int]`

<br/>
Maybe you noticed that the effects are not being executed in the same order as their order in the stack declaration.
The effects can indeed be executed in any order. This doesn't mean though that the results will be the same. For example
running `Writer` then `Disjunction` returns `String Xor (A, List[String])` whereas running `Disjunction` then `Writer` returns
`(String Xor A, List[String])`.

This is only possible because of pretty specific implicits definitions in the library to guide Scala type inference towards the
right return types. You can learn more on implicits in the ${"implicits" ~/ OpenClosed} section.

Otherwise you can also learn about ${"other effects" ~/ OutOfTheBox} supported by this library.
"""

  type ReaderInt[X] = Reader[Int, X]
  type WriterString[X] = Writer[String, X]

  type Stack = ReaderInt |: WriterString |: Eval |: NoEffect

  implicit val ReaderIntMember =
    Member.aux[ReaderInt, Stack, WriterString |: Eval |: NoEffect]

  implicit val WriterStringMember =
    Member.aux[WriterString, Stack, ReaderInt |: Eval |: NoEffect]

  implicit val EvalMember =
    Member.aux[Eval, Stack, ReaderInt |: WriterString |: NoEffect]

}

