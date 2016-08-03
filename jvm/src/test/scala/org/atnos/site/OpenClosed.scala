package org.atnos.site

import org.atnos.eff._, all._
import cats.data._

object OpenClosed extends UserGuidePage { def is = ("Open - Closed").title ^ s2"""

There are 2 ways to create effectful computations for a given effect `M`.

You can create an **open** union of effects:${snippet {
// '|= ' reads 'is member of '
import MemberIn.|=
import StateEffect._
import WriterEffect._

def putAndTell[R](i: Int)(implicit s: State[Int, ?] |= R, w: Writer[String, ?] |= R): Eff[R, Int] =
  for {
    _ <- put(i)
    _ <- tell("stored " + i)
  } yield i
}}

In this case you don't fix the type of the effect stack to use, you just list the effects that the stack will contain.

This has several advantages:

 - you can add more effects later to the `Eff[R, Int]` action being created

 - the `putAndTell` method can be used in different effect stacks as long as they have the `StateInt` and `WriterString` effects

 - no type annotations are required in the for comprehension

On the other hand:

 - this is verbose if you have lots of methods like this, always operating on the same stack of effects

 - you might want to "seal" the stack to declare exactly with which set of effects you want to be working

In that case you can specify an effect stack:${snippet{
import org.atnos.eff._, all._
import cats.data._

type S = State[Int, ?] |: Writer[String, ?] |: NoEffect

implicit val StateIntMember =
  Member.Member2L[State[Int, ?], Writer[String, ?]]

implicit val WriterStringMember =
  Member.Member2R[State[Int, ?], Writer[String, ?]]

def putAndTell(i: Int): Eff[S, Int] =
  for {
    _ <- put(i)
    _ <- tell("stored "+i)
  } yield i
}}

One major issue with this approach is that you will need to define one implicit for each effect that is member of the stack.
The implicit `StateIntMember` for example declares that:

 - `State[Int, ?]` is a member of `S`

 - if you remove `State[Int, ?]` from `S`, you are left with the `Writer[String, ?] |: NoEffect` stack

<br/>
Now you can learn ${"how to create effects" ~/ CreateEffects}

"""

}
