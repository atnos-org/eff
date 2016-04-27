package org.atnos.site

import cats.data._
import org.atnos.eff._, eff._

object Implicits extends UserGuidePage { def is = "Implicits".title ^ s2"""

Type inference with the Eff monad can be a bit tricky to get right if we want to avoid type annotations. Here are some
tips to help you.

### Running effects with several type parameters

Some effects use 2 type variables, like `Reader` or `Writer`. Those effects need some specific implicit declarations in
 order for implicit resolution to work when running effects in any order (this will be fixed with this
  [Scala compiler fix](https://github.com/scala/scala/pull/5102)). Here is the "template" used for the `Reader` effect: ${snippet{

// define "Member" implicits by using a type T with only one type variable
// instead of Reader which has 2
trait ReaderImplicits extends ReaderImplicits1 {
  implicit def ReaderMemberZero[A]: Member.Aux[Reader[A, ?], Reader[A, ?] |: NoEffect, NoEffect] = {
    type T[X] = Reader[A, X]
    Member.zero[T]
  }

  implicit def ReaderMemberFirst[R <: Effects, A]: Member.Aux[Reader[A, ?], Reader[A, ?] |: R, R] = {
    type T[X] = Reader[A, X]
    Member.first[T, R]
  }
}

trait ReaderImplicits1 {
  implicit def ReaderMemberSuccessor[O[_], R <: Effects, U <: Effects, A](implicit m: Member.Aux[Reader[A, ?], R, U]): Member.Aux[Reader[A, ?], O |: R, O |: U] = {
    type T[X] = Reader[A, X]
    Member.successor[T, O, R, U]
  }
}
}}

Following this "template" will help the type inference when using a `run` method (`runReader` in the case of a `Reader` effect).

Note that you will get these implicits for ${"out of the box effects" ~/ OutOfTheBox} by importing `org.atnos.eff.implicits._`.

### Use context bounds

When creating effects you can always "require" a stack containing the right effects with the `Member` typeclass:${snippet {
import cats.syntax.all._
import org.atnos.eff._
import org.atnos.eff.all._

type StateInt[A] = State[Int, A]
type WriterString[A] = Writer[String, A]

def putAndTell[R](i: Int)(implicit s: StateInt <= R, w: WriterString <= R): Eff[R, Int] =
  for {
    // no type annotations needed!
    _ <- put(i)
    _ <- tell("stored " + i)
  } yield i
}}

You can even use context bounds to make the declaration of `putAndTell` more concise:${snippet{
import cats.syntax.all._
import org.atnos.eff.all._

type StateInt[R] = State[Int, ?] <= R
type WriterString[R] = Writer[String, ?] <= R

def putAndTell[R : StateInt : WriterString](i: Int): Eff[R, Int] =
  for {
    _ <- put(i)
    _ <- tell("stored " + i)
  } yield i

}}

### Creating effects for your own stack

When you create your own effect stack you can give a little help to the compiler by adding `Member.Aux` implicits for each effect
in the stack:${snippet{
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
    // no annotations!
    _ <- put(i)
    _ <- tell("stored "+i)
  } yield i
}}

The implicit `StateIntMember` declares that:

 - `StateInt` is a member of `S`

 - if you remove `StateInt` from `S`, you are left with the `WriterString |: NoEffect` stack

"""

}
