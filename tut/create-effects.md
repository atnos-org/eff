# Creating and implementing effects

New effects can be added to the library pretty easily. Let's create an Effect for `scala.concurrent.Future` for example.

We need

 - a base type. We select `Future[() => A]` (instead of `Future[A]` in order to avoid values to be evaluated straight away)
 - a method to send values of type `A` into `Eff[R, A]`
 - an interpreter

```scala
import scala.concurrent._, duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.specs2.control.eff._
import Eff._
import Effects._
import Interpret._
import cats.data._, Xor._
import Member.<=
import cats.syntax.all._

// create the effect and its interpreter
object FutureEffect {
  type Fut[A] = Future[() => A]

  def future[R, A](a: =>A)(implicit m: Fut <= R): Eff[R, A] =
    send[Fut, R, A](Future(() => a))

  def runFuture[R <: Effects, A, B](atMost: Duration)(effects: Eff[Fut |: R, A]): Eff[R, A] = {
    val recurse = new Recurse[Fut, R, A] {
      def apply[X](m: Fut[X]): X Xor Eff[R, A] =
        Left(Await.result(m.map(_()), atMost))
    }
    interpret1((a: A) => a)(recurse)(effects)
  }
}
```

In the code above:

 - the `future` method uses `Eff.send` to "send" values of a given effect into a larger sum of effects `Eff[R, A]`
 - `runFuture` runs the `Future` by using the `Interpret.interpret1` method
  
Writing interpreters can be a bit tricky, especially to keep them stack-safe. There is no method at the moment for writing
generic stack-safe interpreters but the `Interpret` objects offers several support traits and functions to write some of 
them. In this case, the interpretation doesn't need to pass state around so we can use the `Recurse` trait. This kind of 
implementation is shared by many different monads, like `Reader`, `Eval`, `Option` but not `Writer`, `State` or `List` for 
example.

Then we can use this effect in a computation

```scala
import FutureEffect._

type F = Fut |: NoEffect

implicit def FutMember: Fut <= F =
  Member.MemberNatIsMember

val action: Eff[F, Int] = for {
  a <- future(2)
  b <- future(3)
} yield a + b
```
```scala
scala> run(runFuture(3.seconds)(action))
res5: Int = 5
```        

