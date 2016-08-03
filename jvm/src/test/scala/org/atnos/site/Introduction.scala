package org.atnos.site

import cats.data._
import cats.Eval
import org.atnos.eff._, all._

object Introduction extends UserGuidePage { def is = "Introduction".title ^ s2"""

Extensible effects are an alternative to monad transformers for computing with effects in a functional way.
This library uses a "free-er" monad and an "open-union" of effects to create an "effect stack" as described in
[Oleg Kiselyov's paper](http://okmij.org/ftp/Haskell/extensible/more.pdf).

There are lots of advantages to this approach:

 - operations can be declared as "requiring" an effect, without the need to fix the full stack of effects in advance

 - effects handlers are modular and can be replaced with other implementations as needed (even at runtime)

 - the underlying implementation is performant and stack-safe

 - existing monadic datatypes can be integrated to the library

 - effect stacks can be modified or combined

This is probably very abstract so let's see more precisely what this all means.

### First example

A monadic action is modelled as a value of type `Eff[R, A]` where `R` denotes a set of effects and `A` is the value
returned by the computation, possibly triggering some effects when evaluated.

The effects `R` are modelled by a type-level list of "effect constructors", for example:${snippet{
import cats._, data._
import org.atnos.eff._, all._

type Stack = Reader[Int, ?] |: Writer[String, ?] |:: Eval

}}
The stack `Stack` above declares 3 effects:

 - a `Reader[Int, ?]` effect to access some configuration number of type `Int`

 - a `Writer[String, ?]` effect to log string messages

 - an `Eval` effect to only compute values on demand (a bit like lazy values)

 Note that the last effect of a stack needs to be preceded with `|::` instead of `|:`.

Now we can write a program with those 3 effects, using the primitive operations provided by `ReaderEffect`, `WriterEffect` and `EvalEffect`:${snippet{
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

val program: Eff[Stack, Int] = for {
  // get the configuration
  n <- ask[Stack, Int]

  // log the current configuration value
  _ <- tell[Stack, String]("the required power is "+n)

  // compute the nth power of 2
  a <- delay[Stack, Int](math.pow(2, n.toDouble).toInt)

  // log the result
  _ <- tell[Stack, String]("the result is "+a)
} yield a

// run the action with all the interpreters
// each interpreter running one effect
program.runReader(6).runWriter.runEval.run
}.eval}

As you can see, all the effects of the `Stack` type are being executed one by one:

 1. the `Reader` effect, needing a value to inject
 2. the `Writer` effect, which logs messages
 3. the `Eval` effect to compute the "power of 2 computation"
 4. finally the `NoEffect` effect (provided by the `Eff` object) to get the final value out of `Eff[Stack, Int]`

<br/>
Maybe you noticed that the effects are not being executed in the same order as their order in the stack declaration.
The effects can indeed be executed in any order. This doesn't mean though that the results will be the same. For example
running the `Writer` effect then `Xor` effect returns `String Xor (A, List[String])` whereas running the `Xor` effect
 then the `Writer` effect returns `(String Xor A, List[String])`.

This all works thanks to some implicits definitions guiding Scala type inference towards the
right return types. You can learn more on implicits and how to remove the type annotations in the example above
 in the ${"implicits" ~/ Implicits} section.

You can now get a more detailled presentation of the use of the Eff monad by reading the ${"tutorial" ~/ Tutorial} or
you can learn about ${"other effects" ~/ OutOfTheBox} supported by this library.
"""

  type Stack = Reader[Int, ?] |: Writer[String, ?] |: Eval |: NoEffect


}

