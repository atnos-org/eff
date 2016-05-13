package org.atnos.site

import scala.concurrent.duration, duration._
import org.atnos.eff._, all._
import snippets._, FutEffectSnippet._, FutEffect._

object CreateEffects extends UserGuidePage { def is = "Creating effects".title ^ s2"""

### Creation

New effects can be added to the library pretty easily. Let's create an Effect for `scala.concurrent.Future` for example.

We need:

 - a base type. We select `Future[A]` (instead of `Future[A]` in order to avoid values to be evaluated straight away)

 - a method to send values of type `A` into `Eff[R, A]`

 - an interpreter

${definition[FutEffectSnippet]}

In the code above:

 - the `fut` method uses `Eff.send` to "send" values of a given effect into a larger sum of effects `Eff[R, A]`

 - `runFuture` runs the `Future` by using the `Interpret.interpret1` method

Writing interpreters can be a bit tricky, especially to keep them stack-safe. There is no method at the moment for writing
generic stack-safe interpreters but the `Interpret` objects offers several support traits and functions to write some of
them. In this case, the interpretation doesn't need to pass state around so we can use the `Recurse` trait. This kind of
implementation is shared by many different monads, like `Reader`, `Eval`, `Option` but not `Writer`, `State` or `List` for
example.

The `runFuture` method needs an implicit `Member.Aux[Fut, R, U]`. This must be read in the following way:

 - `Fut` must be member of the effect stack `R` and its removal from `R` should be the effect stack `U`

<br/>

Then we can use this effect in a computation:${snippet{

type F = Fut |: NoEffect

val action: Eff[F, Int] = for {
  a <- fut(2)
  b <- fut(3)
} yield a + b

run(runFuture(3.seconds)(action))
}.eval}

"""


}

