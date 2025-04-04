package org.atnos.site

import cats.Eval
import org.atnos.eff.Fx
import org.atnos.site.snippets.*
import org.atnos.site.snippets.HadoopS3Snippet.*

object TransformStack extends UserGuidePage {
  def is = "Transform stacks".title ^ s2"""

### What is an "effect stack"?

There is an abuse of language here. The name "stack" comes from "monad stack" used when talking about monad transformers.
With `Eff` though, effects are modelled differently, as a tree of effects.

For example the type level representation of four effects `T1, T2, T3, T4` is represented as:
```
Fx.fx4[T1, T2, T3, T4]

// or

FxAppend[
  Fx1[T1],
  Fx3[T2, T3, T4]
]
```

So every-time we manipulate effects at the type level we modify a tree of effects. For example, interpreting the effect `T3`
would leave us with the tree:
```
FxAppend[
  Fx1[T1],
  Fx2[T2, T4]
]
```

This code should prove it:
${snippet {
      // for now the following implicit summoning crashes the compiler
      // val member_ : Member.Aux[T3, FxAppend[Fx1[T1], Fx3[T2, T3, T4]], FxAppend[Fx1[T1], Fx2[T2, T4]]] =
      //  implicitly[Member.Aux[T3, FxAppend[Fx1[T1], Fx3[T2, T3, T4]], FxAppend[Fx1[T1], Fx2[T2, T4]]]]
    }}

Unfortunately the compiler has some difficulties with it, so you can either get the member value by using the
  implicit definitions "manually" or you can just summon the member instance without the `Aux` part:
${snippet {
      import org.atnos.eff._

// so you need to explicitly define the implicit
      val member_ : Member.Aux[T3, FxAppend[Fx1[T1], Fx3[T2, T3, T4]], FxAppend[Fx1[T1], Fx2[T2, T4]]] =
        Member.MemberAppendR(using Member.Member3M)

// but this works
      val member: Member[T3, FxAppend[Fx1[T1], Fx3[T2, T3, T4]]] =
        summon[Member[T3, FxAppend[Fx1[T1], Fx3[T2, T3, T4]]]]
    }}

More importantly the compiler is still able to track the right types resulting of the interpretation of a given effect
so the following compiles ok:

${snippet {
      import org.atnos.eff._

      def runT3[R, U, A](e: Eff[R, A])(using Member.Aux[T3, R, U]): Eff[U, A] = ???
      def runT2[R, U, A](e: Eff[R, A])(using Member.Aux[T2, R, U]): Eff[U, A] = ???
      def runT1[R, U, A](e: Eff[R, A])(using Member.Aux[T1, R, U]): Eff[U, A] = ???
      def runT4[R, U, A](e: Eff[R, A])(using Member.Aux[T4, R, U]): Eff[U, A] = ???

      type S = FxAppend[Fx1[T1], Fx3[T2, T3, T4]]

      runT1(runT4(runT2(runT3(Eff.send[T3, S, Int](???)))))

    }}

### Transform an effect to another

#### Change the effect

A typical use case for this is to transform a stack having a `Reader[S, *]` effect to a stack having a `Reader[B, *]` effect
 where `S` is "contained" in `B` (meaning that there is a mapping from `B`, "big", to `S`, "small"). Here is an example:${snippet {
      import org.atnos.eff._, all._
      import org.atnos.eff.syntax.all.given
      import cats._
      import cats.data._

      case class Conf(host: String, port: Int)

      type ReaderPort[A] = Reader[Int, A]
      type ReaderHost[A] = Reader[String, A]
      type ReaderConf[A] = Reader[Conf, A]

      type S1 = Fx.fx2[ReaderHost, Option]
      type S2 = Fx.fx2[ReaderPort, Option]
      type SS = Fx.fx2[ReaderConf, Option]

      val readHost: Eff[S1, String] = for {
        c <- ask[S1, String]
        h <- OptionEffect.some[S1, String]("hello")
      } yield h

      val readPort: Eff[S2, String] = for {
        c <- ask[S2, Int]
        h <- OptionEffect.some[S2, String]("world")
      } yield h

      val fromHost = new (ReaderHost ~> ReaderConf) {
        def apply[X](r: ReaderHost[X]) = Reader((c: Conf) => r.run(c.host))
      }

      val fromPort = new (ReaderPort ~> ReaderConf) {
        def apply[X](r: ReaderPort[X]) = Reader((c: Conf) => r.run(c.port))
      }

      val action: Eff[SS, String] = for {
        s1 <- readHost.transform(fromHost)
        s2 <- readPort.transform(fromPort)
      } yield s1 + " " + s2

      action.runReader(Conf("www.me.com", 8080)).runOption.run
    }.eval}

There are also specialized versions of `transform` for `Reader` and `State`:

 - `ReaderEffect.localReader` takes a "getter" `B => A` to transform a stack with a `Reader[A, *]` into a stack with a `Reader[B, *]`
 - `StateEffect.lensState` takes a "getter" `S => T` and a "setter" `(S, T) => S` to to transform a stack with a `State[T, *]` into a stack with a `State[S, *]`

### Translate an effect into multiple others

A common thing to do is to translate effects (a webservice DSL for example) into multiple others (`TimedFuture`, `Eval`, `Either`, etc...).

For example you might have this stack:
```
type S = Fx.fx3[Authenticated, TimedFuture, Either[AuthError, *]]
```

And you want to write an interpreter which will translate authentication actions into `TimedFuture` and `Either`:${snippet {
      import org.atnos.eff._
      import org.atnos.eff.syntax.eff.given
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
      type _authenticate[U] = Authenticated |= U

      type AuthErroEither[A] = Either[AuthError, A]
      type _error[U] = AuthErroEither |= U

      /**
 * The order of implicit parameters is really important for type inference!
 * see below
 */
      def runAuth[R, U, A](e: Eff[R, A])(using Member.Aux[Authenticated, R, U], _future[U], _error[U]): Eff[U, A] =
        translate(e)(new Translate[Authenticated, U] {
          def apply[X](ax: Authenticated[X]): Eff[U, X] =
            ax match {
              case Authenticate(token) =>
                // send the TimedFuture effect in the stack U
                fromFuture(authenticateImpl(token)).
                  // send the Either value in the stack U
                collapse
            }
        })

// call to a service to authenticate tokens
      def authenticateImpl(token: String): Future[Either[AuthError, AccessRights]] =
        Future.successful[Either[AuthError, AccessRights]] { Left(AuthError("token invalid!")) }

      def authenticate[S: _authenticate](token: String) = Authenticate(token).send

      type S1 = Fx.fx3[Authenticated, Either[AuthError, *], TimedFuture]
      type R1 = Fx.fx2[Either[AuthError, *], TimedFuture]

      val result: Eff[R1, AccessRights] = runAuth(authenticate[S1]("faketoken"))

    }}

The call to `send` above needs to send an `TimedFuture` value in the stack `U`. This is possible because `TimedFuture` is an
effect in `U` as evidenced by `future`.

Furthermore, `authenticate` returns an `Either[AuthError, *]` value. We can "collapse" it into `U` because `Either[AuthError, *]`
is an effect of `U` as evidenced by `either`.


You might wonder why we don't use a more direct type signature like:
```
def runAuth2[R, U :_future :_error, A](e: Eff[R, A])(
  implicit authenticated: Member.Aux[Authenticated, R, U]): Eff[U, A]
```

The reason is that scalac desugars this to:
```
def runAuth2[R, U, A](e: Eff[R, A])(
  implicit future:        _future[U],
           either:        _error[U],
           authenticated: Member.Aux[Authenticated, R, U]): Eff[U, A] =
```

And then `authenticated` is last in the list of implicits parameters and can not be used to guide type inference.

### Interpret an effect "locally"

Let's say you have a method to run database queries
${snippet {
      import org.atnos.eff._
      import org.atnos.eff.all._
      import cats.data._

      trait Db[A]
      type _writerString[R] = Writer[String, *] |= R

      def runDb[R, U, A](queries: Eff[R, A])(using Member.Aux[Db, R, U], _eval[U], _writerString[U]): Eff[U, A] = ???
    }}

The database queries (the `Db` effect) are being executed by the `runDb` method inside the `Eval` effect, and they use
 a `WriterString` effect to log what is being executed.

However you know that some clients of your component don't care about the logs and they don't want to have the `WriterString` effect.
 that they consider an implementation detail.

So you'd like to provide this additional method:
${snippet {
// 8<--
      import org.atnos.eff._
      import org.atnos.eff.all._
      import cats.data._

      trait Db[A]
      type WriterString[A] = Writer[String, A]
      type _writerString[R] = WriterString |= R
// 8<--

      def executeOnDb[R, U, A](queries: Eff[R, A])(using Member.Aux[Db, R, U], _eval[U]): Eff[U, A] = ???
    }}

How can you implement `executeOnDb` with `runDb`?
${snippet {
      // 8<--
      import org.atnos.eff._
      import org.atnos.eff.all._
      import cats.data._

      trait Db[A]
      type WriterString[A] = Writer[String, A]
      type _writerString[R] = WriterString |= R

      def runDb[R, U, A](queries: Eff[R, A])(using Member.Aux[Db, R, U], _eval[U], _writerString[U]): Eff[U, A] = ???
// 8<--
      import org.atnos.eff.syntax.all.given

      def executeOnDb[R, U, A](queries: Eff[R, A])(using Member.Aux[Db, R, U], _eval[U]): Eff[U, A] = {

        type S = Fx.prepend[WriterString, R]
        runDb(queries.into[S]).runWriterNoLog[String]

      }
    }}

You create a "local" stack containing the `WriterString` effect using the `prepend` method. You now run the `Db` effect and
discard the logs to finally return only `Eff[U, A]`.

### Merge stacks

We can create effects for a given effect stack, for example to interact with a [Hadoop](https://hadoop.apache.org) cluster.
We can also define another stack, for storing and retrieving data on [S3](https://aws.amazon.com/s3).
  ${definition[HadoopS3Snippet]}

So what happens when you want to both use S3 and Hadoop? As you can see from the definition above those 2 stacks share
some common effects, so the resulting stack we want to work with is:${snippet {
      import cats.Eval
      import HadoopStack._
      import S3Stack.{WriterString as _, _}

      type HadoopS3 = Fx.fx4[S3Reader, HadoopReader, WriterString, Eval]
    }}

Then we can use the `into` method to inject effects from each stack into this common stack:${snippet {
// 8<--
      type HadoopS3 = Fx.fx4[S3Stack.S3Reader, HadoopStack.HadoopReader, cats.data.Writer[String, *], Eval]
// 8<--
      import S3Stack._
      import HadoopStack._
// this imports the `into` and runXXX syntax
      import org.atnos.eff.syntax.all.given

      val action = for {
        // read a file from hadoop
        s <- readFile("/tmp/data").into[HadoopS3]

        // write a file on S3
        _ <- writeFile("key", s).into[HadoopS3]
      } yield ()

      // and we can run the composite action
      action.runReader(S3Conf("bucket")).runReader(HadoopConf(10)).runWriter.runEval.run
    }.eval}

You can find a fully working example of this approach in `src/test/org/atnos/example/StacksSpec`.
"""

}

// 8<---
trait T1[A]
trait T2[A]
trait T3[A]
trait T4[A]
// 8<---
