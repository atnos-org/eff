# Out of the box

This library comes with a few available effects (in `org.specs2.control.eff._`):

 Name                | Description
 ------------------- | ---------- 
 `EvalEffect`        | an effect for delayed computations
 `OptionEffect`      | an effect for optional computations, stopping when there's no available value
 `DisjunctionEffect` | an effect for computations with failures, stopping when there is a failure
 `ErrorEffect`       | a mix of Eval and Disjunction, catching exceptions and returning them as failures
 `ReaderEffect`      | an effect for depending on a configuration or an environment
 `WriterEffect`      | an effect to log messages
 `StateEffect`       | an effect to pass state around
 `ListEffect`        | an effect for computations returning several values (for non-determinism)

Each object provides methods to create effects and to interpret them.

## Eval

This effect is a very simple one. It allows the delayed execution of computations and it can serve as some sort of overall `IO` effect.

Two methods are available to execute this effect: 

 - `runEval[R <: Effects, A](r: Eff[Eval[?] |: R, A]): Eff[R, A]` to just execute the computations
 - `attemptEval[R <: Effects, A](r: Eff[Eval[?] |: R, A]): Eff[R, Throwable \/ A]` to execute the computations but also catch any `Throwable` that would be thrown
  
## Option

Adding an `Option` effect in your stack allows to stop computations when necessary. 
If you create a value with `some(a)` this value will be used downstream but if you use `none` all computations will stop
```tut:silent
import org.specs2.control.eff._
import Eff._
import Effects._
import OptionEffect._
import cats.syntax.all._

/**
 * Stack declaration
 */
type S = Option |: NoEffect

implicit def OptionMember: Member[Option, S] =
  Member.MemberNatIsMember

// compute with this stack
val map: Map[String, Int] = 
  Map("key1" -> 10, "key2" -> 20) 

val addKeys: Eff[S, Int] = for {
  a <- fromOption(map.get("key1"))
  b <- fromOption(map.get("key2"))
} yield a + b

val addKeysWithMissingKey: Eff[S, Int] = for {
  a <- fromOption(map.get("key1"))
  b <- fromOption(map.get("missing"))
} yield a + b
```
```tut
run(runOption(addKeys))

run(runOption(addKeysWithMissingKey))
```

## Disjunction

The `Disjunction` effect is similar to the `Option` effect but adds the possibility to specify why a computation stopped.
```tut:silent
import org.specs2.control.eff._
import Eff._
import Effects._
import DisjunctionEffect._
import cats.syntax.all._
import cats.data.Xor

/**
 * Stack declaration
 */
type XorString[A] = String Xor A 
type S = XorString |: NoEffect

implicit def XorStringMember: Member[XorString, S] =
  Member.MemberNatIsMember

// compute with this stack
val map: Map[String, Int] = 
  Map("key1" -> 10, "key2" -> 20) 

val addKeys: Eff[S, Int] = for {
  a <- fromOption(map.get("key1"), "'key1' not found")
  b <- fromOption(map.get("key2"), "'key2' not found")
} yield a + b

val addKeysWithMissingKey: Eff[S, Int] = for {
  a <- fromOption(map.get("key1"),    "'key1' not found")
  b <- fromOption(map.get("missing"), "'missing' not found")
} yield a + b
```
```tut
run(runDisjunction(addKeys))

run(runDisjunction(addKeysWithMissingKey))
```