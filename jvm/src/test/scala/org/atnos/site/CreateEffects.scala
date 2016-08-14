package org.atnos.site

import scala.concurrent.duration, duration._
import org.atnos.eff._, all._
import snippets._, FutEffectSnippet._, FutEffect._

object CreateEffects extends UserGuidePage { def is = "Creating effects".title ^ s2"""

### Creation

New effects can be added to the library pretty easily. Let's create an Effect for `scala.concurrent.Future` for example.

We need:

 - a base type. We select `Future[() => A]` (instead of `Future[A]` in order to avoid values to be evaluated straight away)

 - a method to send values of type `A` into `Eff[R, A]`

 - an interpreter

${definition[FutEffectSnippet]}

In the code above:

 - the `fut` method uses `Eff.send` to "send" values of a given effect into a larger sum of effects `Eff[R, A]`

 - `runFuture` runs the `Future` by using the `interpret.interpret1` method

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

Writing interpreters can be a bit tricky, especially to keep them stack-safe. There is no method at the moment for writing
generic stack-safe interpreters but the `org.atnos.eff.interpret` object offers several support traits and functions to write some of
them. In this case, the interpretation doesn't need to pass state around so we can use the `Recurse` trait. This kind of
implementation is shared by many different monads, like `Reader`, `Eval`, `Option` but not `Writer`, `State` or `List` for
example.

The `runFuture` method needs an implicit `Member.Aux[Fut, R, U]`. This must be read in the following way:

 - `Fut` must be member of the effect stack `R` and its removal from `R` should be the effect stack `U`

<br/>

Then we can use this effect in a computation:${snippet{

val action: Eff[Fx.fx1[Fut], Int] =
  for {
    a <- fut(2)
    b <- fut(3)
  } yield a + b

run(runFuture(3.seconds)(action))
}.eval}

----
"""


}

