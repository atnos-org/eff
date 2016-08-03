package org.atnos.site

import cats.data._
import org.specs2.execute.Snippets

object OutOfTheBox extends UserGuidePage { def is = "Out of the box".title ^ s2"""

This library comes with the following effects:

 Name                | Description
 ---------           | --------------------------------------------
 `EvalEffect`        | an effect for delayed computations
 `OptionEffect`      | an effect for optional computations, stopping when there's no available value
 `XorEffect`         | an effect for computations with failures, stopping when there is a failure
 `ValidateEffect`    | an effect for computations with failures, allowing to continue computations and to collect failures
 `ErrorEffect`       | a mix of Eval and Xor, catching exceptions and returning them as failures
 `ReaderEffect`      | an effect for depending on a configuration or an environment
 `WriterEffect`      | an effect to log messages
 `StateEffect`       | an effect to pass state around
 `ListEffect`        | an effect for computations returning several values
 `ChooseEffect`      | an effect for modeling non-determinism
 `FutureEffect`      | an effect using Scala's Future

<small>(from `org.atnos.eff._`)</small>

Each object provides methods to create effects and to interpret them.

### Eval

This effect is a very simple one. It allows the delayed execution of computations and it can serve as some sort of overall `IO` effect.

Two methods are available to execute this effect:

 - `runEval: Eff[U, A]` to just execute the computations

 - `attemptEval: Eff[U, Throwable \/ A]` to execute the computations but also catch any `Throwable` that would be thrown

${snippet{
import org.atnos.eff._, all._, syntax.all._

delay(1 + 1).runEval.run
}.eval}

### Option

Adding an `Option` effect in your stack allows to stop computations when necessary.
If you create a value with `some(a)` this value will be used downstream but if you use `none` all computations will stop:${snippet{
import org.atnos.eff._, all._, syntax.all._

/**
 * Stack declaration
 */
type S = Option |: NoEffect

// compute with this stack
val map: Map[String, Int] =
  Map("key1" -> 10, "key2" -> 20)

// get 2 keys from the map and add the corresponding values
def addKeys(key1: String, key2: String): Eff[S, Int] = for {
  a <- fromOption(map.get(key1))
  b <- fromOption(map.get(key2))
} yield a + b

(addKeys("key1", "key2").runOption.run, addKeys("key1", "missing").runOption.run)
}.eval}

### Xor

The `Xor` effect is similar to the `Option` effect but adds the possibility to specify why a computation stopped: ${snippet{
import org.atnos.eff._, all._, syntax.all._
import cats.data.Xor

/**
 * Stack declaration
 */
type S = (String Xor ?) |: NoEffect

// compute with this stack
val map: Map[String, Int] =
  Map("key1" -> 10, "key2" -> 20)

// get 2 keys from the map and add the corresponding values
def addKeys(key1: String, key2: String): Eff[S, Int] = for {
  a <- optionXor(map.get(key1), s"'$key1' not found")
  b <- optionXor(map.get(key2), s"'$key2' not found")
} yield a + b

(addKeys("key1", "key2").runXor.run, addKeys("key1", "missing").runXor.run)
}.eval}

*Note*: the `?` syntax comes from the [kind-projector](https://github.com/non/kind-projector) project and allows us to avoid
type lambdas.

A `catchLeft` method can also be used to intercept an error and possibly recover from it:${snippet{
// 8<--
import org.atnos.eff._, all._, syntax.all._
import cats.data.Xor
// 8<--
case class TooBig(value: Int)
type E = (TooBig Xor ?) |: NoEffect

val i = 7

val value: Eff[E, Int] =
  if (i > 5) left[E, TooBig, Int](TooBig(i))
  else       right[E, TooBig, Int](i)

val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) { case TooBig(k) =>
  if (k < 10) right[E, TooBig, Int](k)
  else        left[E, TooBig, Int](TooBig(k))
}

action.runXor.run ==== Xor.Right(7)
}}

*Note*: the type annotations on `left` and `right` can be avoided by adding an implicit declaration in scope. You can learn
more about this in the ${"Implicits" ~/ Implicits} section.

### Validate

The `Validate` effect is similar to the `Xor` effect but let you accumulate failures: ${snippet{
import org.atnos.eff._, all._, syntax.all._

/**
 * Stack declaration
 */
type S = Validate[String, ?] |: NoEffect

def checkPositiveInt(i: Int): Eff[S, Unit] =
  validateCheck(i >= 0, s"$i is not positive")

def checkPositiveInts(a: Int, b: Int, c: Int): Eff[S, (Int, Int, Int)] = for {
  _ <- checkPositiveInt(a)
  _ <- checkPositiveInt(b)
  _ <- checkPositiveInt(c)
} yield (a, b, c)

checkPositiveInts(1, -3, -2).runNel.run
}.eval}


### Error

The `Error` effect is both an `Eval` effect and a `Xor` one with `Throwable Xor F` on the "left" side.
 The idea is to represent computations which can fail, either with an exception or a failure. You can:

 - create delayed computations with `ok`

 - fail with `fail(f: F)` where `F` is the failure type

 - throw an exception with `exception`

Other useful combinators are available:

 - `andFinally(last)` registers an action to be executed even in case of an exception or a failure

 - `orElse` runs an action and then run another one if the first is not successful

 - `whenFailed` does the same thing than `orElse` but uses the error for `action1` to decide which action to run next

When you run an `Error` effect you get back an `Error Xor A` where `Error` is a type alias for `Throwable Xor Failure`.

The `Error` object implements this effect with `String` as the `Failure` type but you are encouraged to create our own
failure datatype and extend the `Error[MyFailureDatatype]` trait.

### Reader

The `Reader` effect is used to request values from an "environment". The main method is `ask` to get the current environment
(or "configuration" if you prefer to see it that way) and you can run an effect stack containing a `Reader` effect by
providing a value for the environment with the `runReader` method.

You can also inject a "local" reader into a "bigger" one:${snippet {
import org.atnos.eff._, all._, syntax.all._
import cats.data._

case class Conf(host: String, port: Int)

type R1[A] = Reader[Int, A]
type R2[A] = Reader[Conf, A]

type S = R1 |: NoEffect

val getPort: Eff[S, String] = for {
  p1 <- ask[S, Int]
} yield "the port is " + p1

getPort.localReader((_: Conf).port).runReader(Conf("prod", 80)).run
}.eval}

### Writer

The `Writer` effect is classically used to log values alongside computations. However it generally suffers from the following drawbacks:

 - values have to be accumulated in some sort of `Monoid`

 - it can not really be used to log values to a file because all the values are being kept in memory until the
  computation ends

The `Writer` effect has none of these issues. When you want to log a value you simply `tell` it and when you run the effect
you can select exactly the strategy you want:

  - `runWriter` simply accumulates the values in a `List` which is ok if you don't have too many of them

  - `runWriterFold` uses a `Fold` to act on each value, keeping some internal state between each invocation

You can then define your own custom `Fold` to log the values to a file:${snippet{
import org.atnos.eff._, all._, syntax.all._
import java.io.PrintWriter

type S = Writer[String, ?] |: NoEffect

val action: Eff[S, Int] = for {
 a <- EffMonad[S].pure(1)
 _ <- tell("first value "+a)
 b <- EffMonad[S].pure(2)
 _ <- tell("second value "+b)

} yield a + b

// define a fold to output values
def fileFold(path: String) = new Fold[String, Unit] {
  type S = PrintWriter
  val init: S = new PrintWriter(path)

  def fold(a: String, s: S): S =
    { s.println(a); s }

  def finalize(s: S): Unit =
    s.close
}

action.runWriterFold(fileFold("target/log")).run
io.Source.fromFile("target/log").getLines.toList
}.eval}

### State

A `State` effect can be seen as the combination of both a `Reader` and a `Writer` with these operations:

 - `get` get the current state

 - `put` set a new state

Let's see an example showing that we can also use tags to track different states at the same time:${snippet{
import cats.data._
import org.atnos.eff._, all._, syntax.all._

type S1[A] = State[Int, A]
type S2[A] = State[String, A]

type S = S1 |: S2 |: NoEffect

val swapVariables: Eff[S, String] = for {
  v1 <- get[S, Int]
  v2 <- get[S, String]
  _  <- put[S, Int](v2.size)
  _  <- put[S, String](v1.toString)
  w1 <- get[S, Int]
  w2 <- get[S, String]
} yield "initial: "+(v1, v2).toString+", final: "+(w1, w2).toString

swapVariables.evalState(10).evalState("hello").run
}.eval}

In the example above we have used an `eval` method to get the `A` in `Eff[R, A]` but it is also possible to get both the
 value and the state with `run` or only the state with `exec`.

Instead of tagging state effects it is also possible to transform a State effect acting on a "small" state into a State
effect acting on a "bigger" state:${snippet{
import org.atnos.eff._, all._, syntax.all._

type Count[A] = State[Int, A]
type Sum[A] = State[Int, A]
type Mean[A] = State[(Int, Int), A]

type S1 = Count |: NoEffect
type S2 = Sum |: NoEffect
type S = Mean |: NoEffect

def count(list: List[Int]): Eff[S1, String] = for {
  _ <- put(list.size)
} yield s"there are ${list.size} values"

def sum(list: List[Int]): Eff[S2, String] = {
  val s = if (list.isEmpty) 0 else list.sum
  for {
    _ <- put(s)
  } yield s"the sum is $s"
}

def mean(list: List[Int]): Eff[S, String] = for {
  m1 <- count(list).lensState((_:(Int, Int))._1, (s: (Int,Int), i: Int) => (i, s._2))
  m2 <- sum(list).lensState((_:(Int, Int))._2, (s: (Int, Int), i: Int) => (s._1, i))
} yield m1+"\n"+m2

mean(List(1, 2, 3)).runState((0, 0)).run
}.eval}

### List

The `List` effect is used for computations which may return several values.
 A simple example using this effect would be:${ListSnippets.snippet1}


### Choose

The `Choose` effect is used for non-deterministic computations. With the `Choose` effect you can model computations which either:

  - return no result at all
  - choose between 2 different computations

`Choose` is actually a generalization of `List` where instead of "exploring" all the branches we might "cut" some of them.
That behaviour is controlled by the `Alternative[F]` instance you use when running `Choose`.

For example if we take `List` to run a similar example as before, we get the list of all the accepted pairs:${ChooseSnippets.snippet1}

### Future

The `Future` effect uses Scala`s `Future` for asynchronous computations. You can create values with:

 - `sync`: a "pure" value
 - `async`: a `Future` value

You can then run the `Eff` computation containing a `Future` effect by using either:

 - `awaitFuture(atMost: FiniteDuration)` which might return a timeout exception
 - `detach` to get back a `Future[A]` if the future effect is the last one to be interpreted in the stack of effects

Note that it is possible to delay the execution of `Future` values by having both `Eval` and `Future` in the stack:

 - `delay(10).flatMap(async)`

<br/>
Now you can learn how to  ${"create your own effects" ~/ CreateEffects}.

"""

}

object ListSnippets extends Snippets {
  val snippet1 = snippet{
import org.atnos.eff._, all._, syntax.all._

type S = List |: NoEffect

// create all the possible pairs for a given list
// where the sum is greater than a value
def pairsBiggerThan(list: List[Int], n: Int): Eff[S, (Int, Int)] = for {
  a <- values(list:_*)
  b <- values(list:_*)
  found <- if (a + b > n) singleton((a, b))
           else           empty
} yield found

pairsBiggerThan(List(1, 2, 3, 4), 5).runList.run
}.eval

}

object ChooseSnippets extends Snippets {
val snippet1 = snippet{
import org.atnos.eff._, all._, syntax.all._

type S = Choose |: NoEffect

// create all the possible pairs for a given list
// where the sum is greater than a value
def pairsBiggerThan(list: List[Int], n: Int): Eff[S, (Int, Int)] = for {
  a <- chooseFrom(list)
  b <- chooseFrom(list)
  found <- if (a + b > n) EffMonad[S].pure((a, b))
           else           zero
} yield found

import cats.std.list._

pairsBiggerThan(List(1, 2, 3, 4), 5).runChoose.run
}.eval

}

