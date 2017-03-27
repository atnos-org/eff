package org.atnos.site

import org.atnos.site.snippets.MaybeEffectSnippet
import MaybeEffectSnippet._

object CreateEffects extends UserGuidePage { def is = "Creating effects".title ^ s2"""

### Creation

New effects can be added to the library pretty easily. Let's create an Effect for a new "optional" type.

We need:

 - a base type. We use a `Maybe` data type with 2 cases `Just` and `Nothing`

 - a method to send values of type `A` into `Eff[R, A]`

 - an interpreter

${definition[MaybeEffectSnippet]}

In the code above:

 - the `just` and `nothing` methods use `Eff.send` to "send" values into a larger sum of effects `Eff[R, A]`

 - `runMaybe` runs the `Maybe` effect by using the `interpret.recurse` and a `Recurser` to translate `Maybe` values into `Option` values

### Compiler limitation

When you create an effect you can define a sealed trait and case classes to represent different possibilities for that effect.
For example for interacting with a database you might create: ${snippet{
trait DatabaseEffect {

  case class Record(fields: List[String])

  sealed trait Db[A]
  case class Get[A](id: Int) extends Db[Record]
  case class Update[A](id: Int, record: Record) extends Db[Record]
}
}}

It is recommended to create the `Db` types **outside** of the `DatabaseEffect` trait. Indeed, during `Member` implicit resolution,
depending on how you import the `Db` effect type (if it is inherited from an object or not) you could experience compiler
crashes :-(.

### Interpreter

Interpreting a given effect generally means knowing what to do with a value of type `M[X]` where `M` is the effect. If
the interpreter can "execute" the effect: produce logs (`Writer`), execute asynchronously (`Future`), check the value (`Either`),...
then extract a value `X`, then we can call a continuation to get the next effect and interpret it as well.

The `org.atnos.eff.interpret` object offers several support traits and functions to write interpreters. In this example we
use a `Recurser` which will be used to "extract" a value `X` from `Maybe[X]` or just give up with `Eff.pure(None)`

The `runMaybe` method needs an implicit `Member.Aux[Maybe, R, U]`. This must be read in the following way:

 - `Maybe` must be member of the effect stack `R` and its removal from `R` should be the effect stack `U`

<br/>

Then we can use this effect in a computation:${snippet{
import org.atnos.eff._
import org.atnos.eff.eff._
import MaybeEffect._

val action: Eff[Fx.fx1[Maybe], Int] =
  for {
    a <- just(2)
    b <- just(3)
  } yield a + b

run(runMaybe(action))
}.eval}

----
"""


}

