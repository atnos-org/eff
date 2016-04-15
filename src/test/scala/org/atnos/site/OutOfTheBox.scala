package org.atnos.site

import org.atnos.eff._, all._, implicits._
import cats.syntax.all._
import cats.data._
import Tag._
import org.specs2.execute.Snippets

object OutOfTheBox extends UserGuidePage { def is = "Out of the box".title ^ s2"""

This library comes with a few available effects:

 Name                | Description
 ---------           | --------------------------------------------
 `EvalEffect`        | an effect for delayed computations
 `OptionEffect`      | an effect for optional computations, stopping when there's no available value
 `DisjunctionEffect` | an effect for computations with failures, stopping when there is a failure
 `ErrorEffect`       | a mix of Eval and Disjunction, catching exceptions and returning them as failures
 `ReaderEffect`      | an effect for depending on a configuration or an environment
 `WriterEffect`      | an effect to log messages
 `StateEffect`       | an effect to pass state around
 `ListEffect`        | an effect for computations returning several values
 `ChooseEffect`      | an effect for modeling non-determinism

<small>(from `org.atnos.eff._`)</small>

Each object provides methods to create effects and to interpret them.

### Eval

This effect is a very simple one. It allows the delayed execution of computations and it can serve as some sort of overall `IO` effect.

Two methods are available to execute this effect:

 - `runEval[R <: Effects, A](r: Eff[Eval[?] |: R, A]): Eff[R, A]` to just execute the computations

 - `attemptEval[R <: Effects, A](r: Eff[Eval[?] |: R, A]): Eff[R, Throwable \/ A]` to execute the computations but also catch any `Throwable` that would be thrown

*Note*: the `?` syntax comes from the [kind-projector](https://github.com/non/kind-projector) project and allows us to avoid
type lambdas.

### Option

Adding an `Option` effect in your stack allows to stop computations when necessary.
If you create a value with `some(a)` this value will be used downstream but if you use `none` all computations will stop:${snippet{
import org.atnos.eff._, all._
import cats.syntax.all._

/**
 * Stack declaration
 */
type S = Option |: NoEffect

// compute with this stack
val map: Map[String, Int] =
  Map("key1" -> 10, "key2" -> 20)

// get 2 keys from the map and add the corresponding values
def addKeys(key1: String, key2: String): Eff[S, Int] = for {
  a <- option(map.get(key1))
  b <- option(map.get(key2))
} yield a + b

(run(runOption(addKeys("key1", "key2"))), run(runOption(addKeys("key1", "missing"))))
}.eval}

### Disjunction

The `Disjunction` effect is similar to the `Option` effect but adds the possibility to specify why a computation stopped: ${snippet{
import org.atnos.eff._, all._
import cats.syntax.all._
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
  a <- fromOption(map.get(key1), s"'$key1' not found")
  b <- fromOption(map.get(key2), s"'$key2' not found")
} yield a + b

(run(runDisjunction(addKeys("key1", "key2"))), run(runDisjunction(addKeys("key1", "missing"))))
}.eval}

A `catchLeft` method can also be used to intercept an error and possible recover from it:${snippet{
// 8<--
import org.atnos.eff._, all._
import cats.data.Xor
// 8<--
case class TooBig(value: Int)
type E = (TooBig Xor ?) |: NoEffect

val i = 7

val value: Eff[E, Int] =
  if (i > 5) DisjunctionEffect.left[E, TooBig, Int](TooBig(i))
  else       DisjunctionEffect.right[E, TooBig, Int](i)

val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) { case TooBig(k) =>
  if (k < 10) DisjunctionEffect.right[E, TooBig, Int](k)
  else        DisjunctionEffect.left[E, TooBig, Int](TooBig(k))
}

run(runDisjunction(action)) ==== Xor.Right(7)
}}

### Error

The `Error` effect is both an `Eval` effect and a `Disjunction` one with `Throwable Xor F` on the "left" side.
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
failure datatype and extends the `Error[MyFailureDatatype]` trait.

### Reader

The `Reader` effect is used to request values from an "environment". The main method is `ask` to get the current environment (or "configuration" if you prefer to see it that way)
and you can run an effect stack containing a `Reader` effect by providing a value for the environment with the `runReader` method.

It is also possible to query several independent environments in the same effect stack by "tagging" them:${snippet{
import Tag._
import cats.data._

trait Port1
trait Port2

type R1[A] = Reader[Int, A] @@ Port1
type R2[A] = Reader[Int, A] @@ Port2

type S = R1 |: R2 |: NoEffect

  val getPorts: Eff[S, String] = for {
  p1 <- askTagged[S, Port1, Int]
  p2 <- askTagged[S, Port2, Int]
} yield "port1 is "+p1+", port2 is "+p2

run(runTaggedReader(50)(runTaggedReader(80)(getPorts)))
}.eval}

### Writer

The `Writer` effect is classically used to log values alongside computations. However it generally suffers from the following drawbacks:

 - values have to be accumulated in some sort of `Monoid`

 - it can not really be used to log values to a file because all the values are being kept in memory until the
  computation ends

The `Writer` effect has none of these issues. When you want to log a value you simply `tell` it and when you run the effect you can select exactly the strategy you want:

  - `runWriter` simply accumulates the values in a `List` which is ok if you don't have too many of them

  - `runWriterFold` uses a `Fold` to act on each value, keeping some internal state between each invocation

You can then define your own custom `Fold` to log the values to a file:${snippet{

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

run(runWriterFold(action)(fileFold("target/log")))
io.Source.fromFile("target/log").getLines.toList
}.eval}

### State

A `State` effect can be seen as the combination of both a `Reader` and a `Writer` with these operations:

 - `get` get the current state

 - `put` set a new state

Let's see an example showing that we can also use tags to track different states at the same time:${snippet{
import cats.data._

trait Var1
trait Var2

type S1[A] = State[Int, A] @@ Var1
type S2[A] = State[Int, A] @@ Var2

type S = S1 |: S2 |: NoEffect

val swapVariables: Eff[S, String] = for {
  v1 <- getTagged[S, Var1, Int]
  v2 <- getTagged[S, Var2, Int]
  _  <- putTagged[S, Var1, Int](v2)
  _  <- putTagged[S, Var2, Int](v1)
  w1 <- getTagged[S, Var1, Int]
  w2 <- getTagged[S, Var2, Int]
} yield "initial: "+(v1, v2).toString+", final: "+(w1, w2).toString

run(evalTagged(50)(evalTagged(10)(swapVariables)))
}.eval}

In the example above we have used an `eval` method to get the `A` in `Eff[R, A]` but it is also possible to get both the
 value and the state with `run` or only the state with `exec`.

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

<br/>
Now you can learn about ${"open/closed effect stacks" ~/ OpenClosed}.

"""

}

object ListSnippets extends Snippets {
  val snippet1 = snippet{

type S = List |: NoEffect

// create all the possible pairs for a given list
// where the sum is greater than a value
def pairsBiggerThan(list: List[Int], n: Int): Eff[S, (Int, Int)] = for {
  a <- values(list:_*)
  b <- values(list:_*)
  found <- if (a + b > n) singleton((a, b))
           else           empty
} yield found

run(runList(pairsBiggerThan(List(1, 2, 3, 4), 5)))
}.eval

}

object ChooseSnippets extends Snippets {
val snippet1 = snippet{
  import ChooseEffect._
  type S = Choose |: NoEffect

  // create all the possible pairs for a given list
  // where the sum is greater than a value
  def pairsBiggerThan(list: List[Int], n: Int): Eff[S, (Int, Int)] = for {
    a <- choose(list)
    b <- choose(list)
    found <- if (a + b > n) EffMonad[S].pure((a, b))
             else           zero
  } yield found

  import cats.std.list._

  run(runChoose(pairsBiggerThan(List(1, 2, 3, 4), 5)))
}.eval

}

