package org.atnos.site

import cats.data._
import org.atnos.eff._

object Implicits extends UserGuidePage { def is = "Implicits".title ^ s2"""

Type inference with the Eff monad can be a bit tricky to get right if we want to avoid type annotations. Here are some
tips to help you.

### Running effects with several type parameters

Some effects use 2 type variables, like `Reader` or `Writer`. If you want to use those effects in an effect stack you need
to add a compiler plugin to your build:
```
addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.1.0")
```

### Use context bounds and type aliases

When creating effects you can always "require" a stack containing the right effects with the `Member` typeclass:${snippet {
import org.atnos.eff._
import org.atnos.eff.all._

type StateInt[A] = State[Int, A]
type WriterString[A] = Writer[String, A]

// for creating state effects
def putAndTell[R](i: Int)(implicit s: StateInt <= R, w: WriterString <= R): Eff[R, Int] =
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

### Creating effects for your own stack

When you create your own effect stack you can give a little help to the compiler by adding `Member.Aux` implicits for each effect
in the stack:${snippet{
import cats.data._
import org.atnos.eff._
import org.atnos.eff.all._

object S {
  type StateInt[A] = State[Int, A]
  type WriterString[A] = Writer[String, A]

  type S = StateInt |: WriterString |: NoEffect

  // for the first effect of the stack
  implicit val StateIntMember: Member.Aux[StateInt, S, WriterString |: NoEffect] =
    Member.first

  // for the next effect
  implicit val WriterStringMember: Member.Aux[WriterString, S, StateInt |: NoEffect] =
    Member.successor
}

import S._

def putAndTell(i: Int): Eff[S, Int] =
  for {
    // no annotations!
    _ <- put(i)
    _ <- tell("stored "+i)
  } yield i
}}

The implicit `StateIntMember` declares that:

 - `StateInt` is a member of `S`

 - if you remove `StateInt` from `S`, you are left with the `WriterString |: NoEffect` stack


### Effect deduction

A common thing to do is to 

"""

}
