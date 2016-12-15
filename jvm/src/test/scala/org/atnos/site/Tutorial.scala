package org.atnos.site
import org.atnos.site.snippets._
import tutorial._

object Tutorial extends UserGuidePage { def is = "Tutorial".title ^ s2"""

This tutorial is intentionally structured like the [Free monad tutorial for cats](http://typelevel.org/cats/datatypes/freemonad.html)
 so that a side-by-side comparison of the 2 approaches is possible.

### Study your topic

Let's imagine that we want to create a DSL for a key-value store. We want to be able to do three things with keys:

  - put a value into the store, associated with its key.
  - get a value from the store given its key.
  - delete a value from the store given its key.
<p/>

The idea is to write a sequence of these operations in the embedded DSL as a "program",
interpret the "program", and finally execute the "program" to interact with the actual key-value store.

For example:
```
 put("toto", 3)
 get("toto") // returns 3
 delete("toto")
```

But we want:

 - the computation to be represented as a pure, immutable value
 - to separate the creation and execution of the program
 - to be able to support many different methods of execution

### Create an ADT representing your grammar

ADT stands for "Algebraic Data Type". In this context, it refers to a closed set of types which can be combined to
build up complex, recursive values.

We need to create an ADT to represent our key-value operations: ${definition[AdtSnippet]}

### Free your ADT

There are four basic steps to "freeing" the ADT:

 1. Create smart constructors for `KVStore[_]` using `Eff.send`
 1. Build a program out of key-value DSL operations
 1. Build an interpreter for programs of DSL operations
 1. Execute our interpreted program
 1. [optional] add some syntax for the interpreter

### Create smart constructors using `Eff.send`

These methods will let you create `Eff` values for your key-value store "Effect":${definition[AdtCreationSnippet]}

Each method requires the `KVStore` effect to be a member of an "effect stack" `R`. The return values are of type `Eff[R, A]`
 where `R` is a stack of effects possibly containing other effects than key-value store operations and yielding values of
 type `A`.

### Build a program

Now that we can construct values with `KVStore` effects we can use our DSL to write "programs" using a for-comprehension: ${definition[AdtUsageSnippet]}

This looks like a monadic flow. However, it just builds a recursive data structure representing the sequence of operations.

### Write an interpreter for your program

As you may have understood now, `Eff` is used to create an embedded DSL. By itself, this DSL only represents a sequence
 of operations (defined by a recursive data structure); it doesn't produce anything.

`Eff` is a programming language inside your programming language!

So, like any other programming language, we need to interpret our abstract language into concrete values.

To do this, we will use an interpreter transforming our `KVStore` effects using a simple mutable map: ${definition[AdtInterpreterSnippet]}

Please note this interpreter is impure -- it mutates `kvs` and also produces logging output using `println`.
The whole purpose of functional programming isn't to prevent side-effects, it is just to push side-effects to the
boundaries of your system in a well-known and controlled way.

We can also interpret `KVStore` effects differently and delegate the results to other effects in the same stack:

 - `State` for maintaining the map of values
 - `Writer` for logging
 - `E Either ?` for type errors
<p/>
${definition[AdtInterpreterSafeSnippet]}

`Eff` is just a recursive structure that can be seen as sequence of operations producing other operations, with
potentially other effects. In this way it is similar to folding a `List`. We often use folds (e.g. `foldRight`) to obtain
a single value from a list; this recurses over the structure, combining its contents.

The idea behind an `Eff` interpreter is exactly the same. We "fold" the recursive structure by:

 - consuming each operation
 - compiling the operation into a target language
 - computing next operation
<p/>

An important aspect of interpreters is stack-safety. An interpreter evaluates each step of a computation on the stack
then calls itself to evaluate the other steps. The `org.atnos.eff.interpreter` object provides various methods helping you
write a stack-safe interpreter:

 - `interpretUnsafe` makes you define a `SideEffect` trait to return a value `X` from an effect `T[X]`

 - `translate` makes you define a `Translate` trait to "translate" your effect into other effects in the same stack

 - both are specialized version of `interpret1` which makes you define a `Recurse` trait to either return a value `X`
   from an effect or produce another `Eff` computation

### Run your program

The final step is naturally running your program after interpreting it to another `Eff` value. We need to

 - specify a concrete stack of effects containing the effect we want to interpret `Fx.fx1[KVStore]` (just one effect in the stack)
 - call our interpreter to get a `Eff[NoFx, A]` value
 - call a final `run` to get an `A` value
<p/>

Like this: ${snippet{
// 8<---
import AdtSnippet._
import AdtUsageSnippet._
import AdtInterpreterSnippet._

// 8<---

import org.atnos.eff._, syntax.all._

// run the program with the unsafe interpreter
runKVStoreUnsafe(program[Fx.fx1[KVStore]]).run

}.eval}

With the safe interpreter, the process is the same and we need to

 - specify an effect stack definition with all the effects
 - call our "safe" interpreter
 - call interpreters for all the other effects, including the final `NoFx` effect with `run`
<p/>

Like that: ${snippet{
// 8<---
import AdtSnippet._
import AdtUsageSnippet._
import AdtInterpreterSafeSnippet._
// 8<---
import org.atnos.eff._, syntax.all._
import cats._, data._

// run the program with the safe interpreter
type Stack = Fx.fx4[KVStore, Throwable Either ?, State[Map[String, Any], ?], Writer[String, ?]]

val (result, logs) =
  runKVStore(program[Stack]).runEither.evalState(Map.empty[String, Any]).runWriter.run

(result.toString +: logs).mkString("\n")
}.eval}

### Add some syntax

It is nice to be able to "chain" `run` methods with this additional piece of syntax: ${snippet {
// 8<---
import AdtSnippet._
import AdtUsageSnippet._
import AdtInterpreterSafeSnippet._
import org.atnos.eff._, all._, syntax.all._
import cats._, data._

type _writerString[R] = Writer[String, ?] |= R
type _stateMap[R]     = State[Map[String, Any], ?] |= R

type Stack = Fx.fx4[KVStore, Throwable Either ?, State[Map[String, Any], ?], Writer[String, ?]]

// 8<---

implicit class KVStoreOps[R, A](effects: Eff[R, A]) {
  def runStore[U](implicit m: Member.Aux[KVStore, R, U],
                  throwable:_throwableEither[U],
                  writer:_writerString[U],
                  state:_stateMap[U]): Eff[U, A] =
    runKVStore(effects)
}

val (result, logs) =
  program[Stack].runStore.runEither.evalState(Map.empty[String, Any]).runWriter.run

(result.toString +: logs).mkString("\n")

}.eval}

### Composing ADTs with the Eff monad

Real world applications often time combine different algebras. The typelevel set of effects `R` in `Eff[R, A]`
lets us compose different algebras in the context of `Eff`.

Let's see a trivial example of unrelated ADT's getting composed that can form a more complex program. First you define
your ADTs with smart constructors:${definition[UserInteractionSnippet]}

Then you simply require your program to have `MemberIn` instances for those effects:${definition[UserInteractionProgramSnippet]}

Finally we write one interpreter per ADT:${definition[UserInteractionInterpretersSnippet]}

Now if we run our program for a Stack combining both effects and type in "snuggles" when prompted, we see something like this:${snippet{
// 8<--
import UserInteractionSnippet._
import UserInteractionInterpretersSnippet._
import UserInteractionProgramSnippet._
import org.atnos.eff._, syntax.all._
// 8<--

type Stack = Fx.fx2[Interact, DataOp]

runInteract(runDataOp(program[Stack])).run
}}
```
What's the kitty's name?
Current cats: snuggles
```

"""


}

