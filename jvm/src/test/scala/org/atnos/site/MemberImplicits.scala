package org.atnos.site

import cats.data._
import org.atnos.eff._

object MemberImplicits extends UserGuidePage { def is = "Member implicits".title ^ s2"""

Type inference with the Eff monad can be a bit tricky to get right if we want to avoid type annotations. Here are some
tips to help you.

### Running effects with several type parameters

Some effects use 2 type variables, like `Reader` or `Writer`. If you want to use those effects in an effect stack you need
to add a compiler plugin to your build:
```
addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")
```

### Use context bounds and type aliases

When creating effects you can always "require" a stack containing the right effects with the `MemberIn` typeclass:${snippet {
import org.atnos.eff._
import org.atnos.eff.all._

type StateInt[A] = State[Int, A]
type WriterString[A] = Writer[String, A]

// for creating state effects
def putAndTell[R](i: Int)(implicit s: StateInt |= R, w: WriterString |= R): Eff[R, Int] =
  for {
    // no type annotations needed!
    _ <- put(i)
    _ <- tell("stored " + i)
  } yield i
}}

You can even use context bounds to make the declaration of `putAndTell` more concise:${snippet{
import org.atnos.eff.all._

type _stateInt[R] = State[Int, ?] |= R
type _writerString[R] = Writer[String, ?] |= R

def putAndTell[R :_stateInt :_writerString](i: Int): Eff[R, Int] =
  for {
    _ <- put(i)
    _ <- tell("stored " + i)
  } yield i

}}

### Know your `Member` typeclasses

There are 3 different ways to declare that an effect is part of an effect stack with 3 typeclasses:

 Typeclass             | Alias    | Meaning                                            | When to use it
 -----------           | -----    | --------------------                               | --------------------------------
 `MemberIn[M, R]`      | `M |= R` | "`M` is part of `R`"                               | to create `M` effects in `R`
 `MemberInOut[M, R]`   | `M /= R` | "`M` is part of `R` and can be extracted from it"  | to intercept the effect `M` (see `Interpreter.scala`) and transform it while staying in the same stack. For example to `handleError` for an Error effect
 `Member[M, R]`        | `M <= R` | "`M` is part of `R`, can be extracted from it, and the resulting stack is `m.Out`" | to interpret the effect in terms of special values or other effects and remove the effect from the stack
"""

}
