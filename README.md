# eff

[![Build Status](https://travis-ci.org/etorreborre/eff-cats.png?branch=master)](https://travis-ci.org/etorreborre/eff-cats)

Monadic effects in Scala.

This library uses a "free-er" monad and an "open-union" of effects to create monadic "stacks" as described in 
[Oleg Kiselyov's paper](http://okmij.org/ftp/Haskell/extensible/more.pdf).

It can be used as an alternative to monad transformers and offers the following features:

 - operations can be declared as "requiring" an effect, without the need to fix the full stack of effects in advance
 
 - there is an operator to from a given effect stack to another, provided that the target one declares at least the same
   effects as the source one
   
 - effects handlers are modular and can be replaced with other implementations as needed
 
 - the underlying implementation is performant and stack-safe
 
 - existing monads can be integrated in this library
 
This is probably very abstract so let's see more precisely what this all means.

## First example

A monadic action is modelled as a value of type `Eff[R, A]` where `R` denotes a set of effects and `A` is the value
returned by the computation, possibly triggering some effects when evaluated.

The effects `R` are modelled by a type-level list of "Effect constructors", for example:
```scala
 type R = Reader[Int, ?] |: Writer[String, ?] |: Eval |: NoEffect     
```

The stack `R` above declares 3 effects:

 - a `Reader[Int, ?]` effect to access some configuration number of type `Int`
 - a `Writer[String, ?]` effect to log messages
 - an `Eval` effect to only compute values on demand (a bit like byname parameters)
 
Those 3 effects come with the library but it is easy to create your own as well. 
 
A typical computation with such a stack would be:
```scala
import cats._, data._, Xor._
import cats.syntax.all._
import cats.std.all._
import org.specs2.control._
import ReaderEffect._
import WriterEffect._
import EvalEffect._
import Effects._  
import Eff._  

object StackEffects {
  type Stack = ReaderInt |: WriterString |: Eval |: NoEffect

  /**
   * Those declarations are necessary to guide implicit resolution
   * but they only need to be done once per stack
   */
  type ReaderInt[X] = Reader[Int, X]
  type WriterString[X] = Writer[String, X]

  implicit def ReaderMember: Member[ReaderInt, Stack] =
    Member.MemberNatIsMember

  implicit def WriterMember: Member[WriterString, Stack] =
    Member.MemberNatIsMember

  implicit def EvalMember: Member[Eval, Stack] =
    Member.MemberNatIsMember
}

import StackEffects._

// create an action
val action: Eff[Stack, Int] = for {
  // get the configuration
  init <- ask[Stack, Int]

  // log the current configuration value
  _ <- tell[Stack, String]("START: the start value is "+init)

  // compute the nth power of 2
  a <- delay(powerOfTwo(init))

  // log an end message
  _ <- tell[Stack, String]("END")
} yield a

// run the action with all the interpreters
val result: (Int, List[String]) =
  run(runEval(runWriter(runReader(5)(action))))

result === ((32, List("START: the start value is 5", "END")))
```

## Out of the box

This library comes with a few available effects

 Name                | Description
 ------------------- | ---------- 
 EvalEffect          | an effect for delayed computations
 OptionEffect        | an effect for optional computations, stopping when there's no available value
 DisjunctionEffect   | an effect for computations with failures, stopping when there is a failure
 ErrorEffect         | a mix of Eval and Disjunction, catching exceptions and returning them as failures
 ReaderEffect        | an effect for depending on a configuration or an environment
 WriterEffect        | an effect to log messages
 StateEffect         | an effect to pass state around
 ListEffect          | an effect for computations returning several values

  
## Creating and implementing effects

It is possible to create an Effect for `scala.concurrent.Future` for example.
We need

 - a base type. We select `Future[() => A]` to avoid values to be evaluated straight away
 - a method to send values of type `A` into `Eff[R, A]`
 - an interpreter

```scala
import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global
import Interpret._

def futureEffect = {

  // create the effect and its interpreter
  object FutureEffect {

    type Fut[A] = Future[() => A]

    def future[R, A](a: =>A)(implicit m: Fut <= R): Eff[R, A] =
      send[Fut, R, A](Future(() => a))

    def runFuture[R <: Effects, A, B](atMost: Duration)(effects: Eff[Fut |: R, A]): Eff[R, A] = {
      val recurse = new Recurse[Fut, R, A] {
        def apply[X](m: Fut[X]): X Xor Eff[R, A] =
          Left(Await.result(m.map(_()), atMost))
      }
      interpret1((a: A) => a)(recurse)(effects)
    }
  }

  // use the effect in a monadic computation
  import FutureEffect._

  type F = Fut |: NoEffect
  implicit def FutMember: Fut <= F =
    Member.MemberNatIsMember

  val action: Eff[F, Int] = for {
    a <- future(1)
    b <- future(2)
  } yield a + b

  run(runFuture(3.seconds)(action)) ==== 3
```        

Writing interpreters can be a bit tricky, especially to keep them stack-safe. There is no method at the moment for writing
generic stack-safe interpreters but the `Interpret` objects offers several support traits and functions to write some of 
them. In this case, the interpretation doesn't need to pass state around so we can use the `Recurse` trait. This kind of 
implementation is shared by many different monads, like `Reader`, `Eval`, `Option` but not `Writer`, `State` or `List` for 
example.

## Transforming an effect stack into another

There are 2 ways to create effectful computations for a given effect `M`:

 - require a stack `R` and `M <= R` (which is an alias for `implicit m: Member[M, R]`)
```scala
 def askAndTell[R](implicit m: ReaderInt <= M, w: WriterString <= R): Eff[R, Int] = 
   for {
     i <- ask[R, Int]
     _ <- tell[R, String]("using "+i)
   } yield i
 ```
 
 For a given domain, this can a be a bit annoying to annotate all the methods like `askAndTell` with the set of all required
 effect members. So you might want to declare, for that domain, a stack of all the effects you are going to use and directly
 create values for that stack:
```scala
 type RW = ReaderInt |: WriterString |: NoEffect

 def askAndTell: Eff[RW, Int] = 
   for {
     i <- ask[RW, Int]
     _ <- tell("using "+i)
   } yield i
 ```

The trouble, and this also arises with monad transformers, is when you want to work with different stacks having different
sets of effects. In that case you can use the `into` method to "unify" the effects of 2 stacks into 1:
```scala
import org.specs2.control.eff.syntax.eff._

type Hadoop = HadoopReader |: WriterString |: Eval |: NoEffect
type S3     = S3Reader     |: WriterString |: Eval |: NoEffect

type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect

// use both the Hadoop stack and the S3 stack
val action = for {
  s <- readFile("/tmp/data").into[HadoopS3]
  _ <- writeFile("key", s)  .into[HadoopS3]
} yield ()
``` 

You can find a fully working example of this approach in `src/test/org/specs2/example/StacksSpec`. 
