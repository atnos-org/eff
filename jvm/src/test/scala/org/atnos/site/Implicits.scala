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

A common thing to do is to translate "high-level" effects (a webservice DSL for example) into low-level ones (`Future`, `Eval`, `Xor`, etc...).

For example you might have this stack:
```
type S = Authenticated |: Future |: (Throwable Xor ?) |: NoEffect
```

And you want to write an interpreter which will translate authentication actions into `Future` and `Xor`:${snippet{
import org.atnos.eff.eff._
import org.atnos.eff.syntax.eff._
import org.atnos.eff.future._
import org.atnos.eff.interpret._
import scala.concurrent.Future

// list of access rights for a valid token
case class AccessRights(rights: List[String])

// authentication error
case class AuthError(message: String)

// DSL for authenticating users
sealed trait Authenticated[A]
case class Authenticate(token: String) extends Authenticated[AccessRights]

type _error[R] = (AuthError Xor ?) |= R


def runAuth[R :_future :_error, U, A](e: Eff[R, A])(implicit m: Member.Aux[Authenticated, R, U]): Eff[U, A] =
  translate(e) { new Translate[Authenticated, U] {
    def apply[X](ax: Authenticated[X]): Eff[U, X] =
      ax match {
        case Authenticate(token) =>
          // send the future effect in the stack U
          send(authenticate(token))(m.out[Future]).
          // send the Xor value in the stack U
          collapse(m.out[AuthError Xor ?])
      }
  }}

// call to a service to authenticate tokens
def authenticate(token: String): Future[AuthError Xor AccessRights] = ???

}}

The call to `send` above needs to send a `Future` value in the stack `U`. But the type signature of `runAuth` only indicates that:

 - `Future` is an effect in `R`
 - `Authenticated` is an effect in `R` and removing it from `R` leaves us with `U`

We should be able to deduce from those 2 facts that `Future` is also an effect of `U` but we need to show a proof of that.
 This is what `m.out[Future]` does. It builds a `MemberIn[Future, U]` instance which can then be used to inject a `Future`
 into `U`.

There is an implicit conversion from `Member.Aux[O, R, U]` to `MemberIn[T, R]` (when `MemberIn[T, R]` exists) in the
`org.atnos.eff.member` object to avoid calling `m.out` explicitely. However this conversion cannot be used all the time
because it might infer the wrong member instances in some cases.

You might wonder why we don't use a more direct type signature like:
```
def runAuth2[R, U :_future :_error, A](e: Eff[R, A])(implicit m: Member.Aux[Authenticated, R, U]): Eff[U, A] =
```

The reason is that Scala has some difficulty to infer the type parameters when calling `runAuth2` and they have to be explicitly
provided. The first declaration works fine with type inference.

"""

}
