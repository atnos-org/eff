package org.atnos.site

import cats.data._

object OpenClosed extends UserGuidePage { def is = ("Open - Closed").title ^ s2"""

There are 2 ways to create effectful computations for a given effect `M`.

You can create an **open** union of effects:${snippet {
import cats.syntax.all._
import org.atnos.eff._
import org.atnos.eff.all._

type StateInt[A] = State[Int, A]
type WriterString[A] = Writer[String, A]

def putAndTell[R](i: Int)(implicit s: StateInt <= R, w: WriterString <= R): Eff[R, Int] =
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

 - you might want to "seal" the stack to declare exactly with which set of effects you want to be working with

In that case you can specify an effect stack:${snippet{
import cats.data._
import cats.syntax.all._
import org.atnos.eff._
import org.atnos.eff.all._

object S {
  type StateInt[A] = State[Int, A]
  type WriterString[A] = Writer[String, A]

  type S = StateInt |: WriterString |: NoEffect

  implicit val StateIntMember: Member.Aux[StateInt, S, WriterString |: NoEffect] =
    Member.first

  implicit val WriterStringMember: Member.Aux[WriterString, S, StateInt |: NoEffect] =
    Member.successor
}

import S._

def putAndTell(i: Int): Eff[S, Int] =
  for {
    _ <- put(i)
    _ <- tell("stored "+i)
  } yield i
}}

One major issue with this approach is that you will need to use type annotations in the for comprehension unless you
define one implicit for each effect that is member of the stack. The implicit `StateIntMember` for example declares that:

 - `StateInt` is a member of `S`

 - if you remove `StateInt` from `S`, you are left with the `WriterString |: NoEffect` stack

<br/>
Now you can learn ${"how to create effects" ~/ CreateEffects}

"""

}
