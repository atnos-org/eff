package org.atnos.site

import java.util.concurrent.ExecutorService

import cats.data._
import cats.syntax.either._
import org.specs2.execute._

object OutOfTheBox extends UserGuidePage { def is = "Out of the box".title ^ s2"""

This library comes with the following effects:

 Name                | Description
 ---------           | --------------------------------------------
 `EvalEffect`        | an effect for delayed computations
 `OptionEffect`      | an effect for optional computations, stopping when there's no available value
 `EitherEffect`      | an effect for computations with failures, stopping when there is a failure
 `ValidateEffect`    | an effect for computations with failures, allowing to continue computations and to collect failures
 `ErrorEffect`       | a mix of Eval and Either, catching exceptions and returning them as failures
 `ReaderEffect`      | an effect for depending on a configuration or an environment
 `WriterEffect`      | an effect to log messages
 `StateEffect`       | an effect to pass state around
 `ListEffect`        | an effect for computations returning several values
 `ChooseEffect`      | an effect for modeling non-determinism
 `AsyncEffect`       | an effect for asynchronous computations
 `SafeEffect`        | an effect for guaranteeing resource safety

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
type S = Fx.fx1[Option]

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

### Either

The `Either` effect is similar to the `Option` effect but adds the possibility to specify why a computation stopped: ${snippet{
import org.atnos.eff._, all._, syntax.all._

/**
 * Stack declaration
 */
type S = Fx.fx1[String Either ?]

// compute with this stack
val map: Map[String, Int] =
  Map("key1" -> 10, "key2" -> 20)

// get 2 keys from the map and add the corresponding values
def addKeys(key1: String, key2: String): Eff[S, Int] = for {
  a <- optionEither(map.get(key1), s"'$key1' not found")
  b <- optionEither(map.get(key2), s"'$key2' not found")
} yield a + b

(addKeys("key1", "key2").runEither.run, addKeys("key1", "missing").runEither.run)
}.eval}

*Note*: the `?` syntax comes from the [kind-projector](https://github.com/non/kind-projector) project and allows us to avoid
type lambdas.

A `catchLeft` method can also be used to intercept an error and possibly recover from it:${snippet{
// 8<--
import org.atnos.eff._, all._, syntax.all._
// 8<--
case class TooBig(value: Int)
type E = Fx.fx1[TooBig Either ?]

val i = 7

val value: Eff[E, Int] =
  if (i > 5) left[E, TooBig, Int](TooBig(i))
  else       right[E, TooBig, Int](i)

val action: Eff[E, Int] = catchLeft[E, TooBig, Int](value) { case TooBig(k) =>
  if (k < 10) right[E, TooBig, Int](k)
  else        left[E, TooBig, Int](TooBig(k))
}

action.runEither.run ==== Right(7)
}}

*Note*: the type annotations on `left` and `right` can be avoided by adding an implicit declaration in scope. You can learn
more about this in the ${"Implicits" ~/ MemberImplicits} section.

### Validate

The `Validate` effect is similar to the `Either` effect but let you accumulate failures: ${snippet{
import org.atnos.eff._, all._, syntax.all._

/**
 * Stack declaration
 */
type S = Fx.fx1[Validate[String, ?]]

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

The `Error` effect is both an `Eval` effect and a `Either` one with `Throwable Either F` on the "left" side.
 The idea is to represent computations which can fail, either with an exception or a failure. You can:

 - create delayed computations with `ok`

 - fail with `fail(f: F)` where `F` is the failure type

 - throw an exception with `exception`

Other useful combinators are available:

 - `andFinally(last)` registers an action to be executed even in case of an exception or a failure

 - `orElse` runs an action and then run another one if the first is not successful

 - `whenFailed` does the same thing than `orElse` but uses the error for `action1` to decide which action to run next

When you run an `Error` effect you get back an `Error Either A` where `Error` is a type alias for `Throwable Either Failure`.

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

type S = Fx.fx2[R1, R2]

def getPort[R](implicit r: Reader[Int, ?] |= R): Eff[R, String] = for {
  p1 <- ask[R, Int]
} yield "the port is " + p1

getPort[S].localReader((_: Conf).port).runReader(Conf("prod", 80)).run
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

type S = Fx.fx1[Writer[String, ?]]

val action: Eff[S, Int] = for {
 a <- pure[S, Int](1)
 _ <- tell("first value "+a)
 b <- pure[S, Int](2)
 _ <- tell("second value "+b)

} yield a + b

// define a fold to output values
def fileFold(path: String) = new LeftFold[String, Unit] {
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

type S = Fx.fx2[S1, S2]

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
type Sum[A]   = State[Int, A]
type Mean[A]  = State[(Int, Int), A]

type S1 = Fx.fx1[Count]
type S2 = Fx.fx1[Sum]
type S  = Fx.fx1[Mean]

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
 A simple example using this effect would be:
${ListSnippets.snippet1}

### Choose

The `Choose` effect is used for non-deterministic computations. With the `Choose` effect you can model computations which either:

  - return no result at all
  - choose between 2 different computations

`Choose` is actually a generalization of `List` where instead of "exploring" all the branches we might "cut" some of them.
That behaviour is controlled by the `Alternative[F]` instance you use when running `Choose`.

For example if we take `List` to run a similar example as before, we get the list of all the accepted pairs:
${ChooseSnippets.snippet1}

### Async

The Async effect is an "abstract" effect. This means that you can create asynchronous expressions using the same API
and have it implemented using either [Scala `Future`](http://docs.scala-lang.org/overviews/core/futures.html) or
[Scalaz `Task`](http://timperrett.com/2014/07/20/scalaz-task-the-missing-documentation/) or [Monix `Task`](https://monix.io/docs/2x/eval/task.html).

For the following example we will assume an implementation using regular Scala futures.

In order to access the API you first need to create an `AsyncService`:${snippet{

import org.atnos.eff._
import org.atnos.eff.async._
import org.atnos.eff.syntax.all._

import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global

/*p
Then all the `AsyncService` operations are available
 */

type R = Fx.fx2[Async, Option]

val action: Eff[R, Int] =
  for {
    // create a value now
    a <- asyncNow[R, Int](1)

    // evaluate a value later
    b <- asyncDelay[R, Int](1)

    // evaluate a value asynchronously
    c <- asyncFork[R, Int](a + b)
  } yield c

/*p
Finally you can evaluate other effects in the stack (Option here) and run the async effect to get back
 a `Future`
*/

val interpreter = AsyncFutureInterpreter.create
import interpreter._

Await.result(action.runOption.runAsyncFuture, 1 second)
}.eval}

If you prefer to use monix or scalaz you need to add a dependency on `eff-monix` or `eff-scalaz` and create
the corresponding `org.atnos.eff.monix.AsyncTaskService` or `org.atnos.eff.scalaz.AsyncTaskService`:
```
import org.atnos.eff._

import java.util.concurrent._
import scalaz.concurrent._

implicit val es = Strategy.DefaultExecutorService
val scalazService: AsyncService = org.atnos.eff.scalaz.AsyncTaskService.create

// the monix service doesn't require any implicit context!
val monixService: AsyncService = org.atnos.eff.monix.AsyncTaskService.create
```

### Safe

The Safe effect is useful to handle resources which must be closed even in the presence of exceptions. The main
operations are

 - `finally` to create an action which must always be executed after another one, even if the first one fails
 - `catchThrowable` to handle a thrown exception
 - `bracket(open)(step)(close)` to open a resource, use it and then close it safely. The `close` part is a "finalizer"
<br/>

Let's see an example for the protection of a resource: ${snippet{
import org.atnos.eff.syntax.all._
import org.atnos.eff._, all._
import cats.Eval

// let's represent a resource which can be in use
case class Resource(values: List[Int] = (1 to 10).toList, inUse: Boolean = false) {
  def isClosed = !inUse
}

var resource = Resource()

// our stack of effects, with safe evaluation
type S = Fx.fx1[Safe]

def openResource: Eff[S, Resource] =
  protect { resource = resource.copy(inUse = true); resource }

def closeResource(r: Resource): Eff[S, Unit] =
  protect(resource = r.copy(inUse = false))

def useResource(ok: Boolean) = (r: Resource) =>
  protect[S, Int](if (ok) r.values.sum else throw new Exception("boo"))

// this program uses the resource safely even if there is an exception
def program(ok: Boolean): (Throwable Either Int, List[Throwable]) =
  bracket(openResource)(useResource(ok))(closeResource).
    runSafe.run
// 8<--
"Results" +
showResult1("Without exception", program(ok = true),  "resource is closed", resource.isClosed) +
showResult1("With exception   ", program(ok = false), "resource is closed", resource.isClosed)

}.eval}

As you can see in the signature of `program` the return value of `runSafe` is `(Throwable Either A, List[Throwable])`.
The first part is the result of your program, which may end with an exception, and the second part is the list of
possible exceptions thrown by the finalizers which can themselves fail.

A simpler version of `bracket` is `finally`.

This example show how to use `finally` but also what happens if a finalizer fails:${snippet{
import org.atnos.eff.syntax.all._
import org.atnos.eff._, all._

// our stack of effects, with safe evaluation
type S = Fx.fx1[Safe]

var sumDone: Boolean = false

def setDone(ok: Boolean): Eff[S, Unit] =
  protect[S, Unit](if (ok) sumDone = true else throw new Exception("failed!!"))

// this program tries to set sumDone to true when the computation is done
def program(ok: Boolean, finalizeOk: Boolean): (Throwable Either Int, List[Throwable]) =
  (protect[S, Int](if (ok) (1 to 10).sum else throw new Exception("boo")) `finally` setDone(finalizeOk)).
    runSafe.run

// 8<--
"Results" +
  showResult2("Computation ok, finalizer ok", program(ok = true, finalizeOk = true)  ) +
  showResult2("Computation ok, finalizer ko", program(ok = true, finalizeOk = false) ) +
  showResult2("Computation ko, finalizer ok", program(ok = false, finalizeOk = true) ) +
  showResult2("Computation ko, finalizer ko", program(ok = false, finalizeOk = false))
}.eval}

Finally (no pun intended!) note that you can use `execSafe` if you are not interested in the result of the finalizers.

<br/>

## What's next?

Now you can learn how to  ${"create your own effects" ~/ CreateEffects}!

"""

  def showResult1(message: String, result: (Throwable Either Int, List[Throwable]), resourceMessage: String, closed: Boolean) =
    result match {
      case (r, ls) =>
        s"""|
            |$message: ${r.leftMap(_.getMessage)}, finalizers exceptions: ${if (ls.isEmpty) "no exceptions" else ls.map(_.getMessage).toString},\t$resourceMessage: $closed""".stripMargin
    }

  def showResult2(message: String, result: (Throwable Either Int, List[Throwable])) =
    result match {
      case (r, ls) =>
        s"""|
          |$message: ${r.leftMap(_.getMessage)}, finalizers exceptions: ${if (ls.isEmpty) "no exceptions" else ls.map(_.getMessage).toString}""".stripMargin
    }
}

object ListSnippets extends Snippets {
  val snippet1 = snippet {
import org.atnos.eff._, all._, syntax.all._

type S = Fx.fx1[List]

// create all the possible pairs for a given list
// where the sum is greater than a value
def pairsBiggerThan[R :_list](list: List[Int], n: Int): Eff[R, (Int, Int)] = for {
  a <- values(list:_*)
  b <- values(list:_*)
  found <- if (a + b > n) singleton((a, b))
           else           empty
} yield found

pairsBiggerThan[S](List(1, 2, 3, 4), 5).runList.run
}.eval

}

object ChooseSnippets extends Snippets {

  val snippet1 = snippet{
import org.atnos.eff._, all._, syntax.all._

type S = Fx.fx1[Choose]

// create all the possible pairs for a given list
// where the sum is greater than a value
def pairsBiggerThan[R :_choose](list: List[Int], n: Int): Eff[R, (Int, Int)] = for {
  a <- chooseFrom(list)
  b <- chooseFrom(list)
  found <- if (a + b > n) EffMonad[R].pure((a, b))
           else           zero
} yield found

import cats.instances.list._

pairsBiggerThan[S](List(1, 2, 3, 4), 5).runChoose.run
}.eval

}

