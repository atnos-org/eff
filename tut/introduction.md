# Extensible effects in Scala

Extensible effects are an alternative to monad transformers for computing with effects in a functional way. 
This library uses a "free-er" monad and an "open-union" of effects to create an "effect stack" as described in 
[Oleg Kiselyov's paper](http://okmij.org/ftp/Haskell/extensible/more.pdf).

It and offers the following features:

 - operations can be declared as "requiring" an effect, without the need to fix the full stack of effects in advance
 
 - there is an operator to go from a given effect stack to another, provided that the target one declares at least the same
   effects as the source one
   
 - effects handlers are modular and can be replaced with other implementations as needed (even at runtime)
 
 - the underlying implementation is performant and stack-safe
 
 - existing monad datatypes can be integrated to the library
 
This is probably very abstract so let's see more precisely what this all means.

## First example

A monadic action is modelled as a value of type `Eff[R, A]` where `R` denotes a set of effects and `A` is the value
returned by the computation, possibly triggering some effects when evaluated.

The effects `R` are modelled by a type-level list of "Effect constructors", for example:
```scala
import cats.data._
import org.specs2.control.eff._  
import Eff._  
import Effects._  
import EvalEffect._
import ReaderEffect._
import WriterEffect._

type ReaderInt[X] = Reader[Int, X]
type WriterString[X] = Writer[String, X]

type Stack = ReaderInt |: WriterString |: Eval |: NoEffect     
```

The stack `Stack` above declares 3 effects:

 - a `ReaderInt` effect to access some configuration number of type `Int`
 - a `WriterString` effect to log string messages
 - an `Eval` effect to only compute values on demand (a bit like lazy values)
 
Before we can use such a stack we need a bit more boilerplate code: 
```scala
import cats.syntax.all._
import cats.std.all._

/**
 * Those declarations are necessary to guide implicit resolution
 * but they only need to be done once per stack.
 * 
 * Also we need to declare type aliases for Reader and Writer
 * instead of using Reader[Int, ?] or Writer[String, ?] more directly
 */

implicit def ReaderMember: Member[ReaderInt, Stack] =
  Member.MemberNatIsMember

implicit def WriterMember: Member[WriterString, Stack] =
  Member.MemberNatIsMember

implicit def EvalMember: Member[Eval, Stack] =
  Member.MemberNatIsMember
```

Now we can write a program with those 3 effects, using the primitive operations provided by `ReaderEffect`, `WriterEffect` and `EvalEffect`
```scala
val program: Eff[Stack, Int] = for {
  // get the configuration
  init <- ask

  // log the current configuration value
  _ <- tell("START: the start value is "+init)

  // compute the nth power of 2
  a <- delay(math.pow(2, init.toDouble).toInt)

  // log an end message
  _ <- tell("END")
} yield a
```

```scala
scala> // run the action with all the interpreters
     | // each interpreter running one effect
     | run(runEval(runWriter(runReader(6)(program))))
res9: (Int, List[String]) = (64,List(START: the start value is 6, END))
```

As you can see you need to run the effects in the same order as their declaration in the `Stack` type: first the `Reader` effect, needing a value to inject.
Then the `Writer` effect, which will log values. Then, the `Eval` effect to compute the "power of 2 computation" and 
finally the `NoEffect` effect (provided by the `Eff` object) to get the final value out of `Eff[Stack, Int]`. 

Now you can learn about [other effects](out-of-the-box.md) supported by this library
