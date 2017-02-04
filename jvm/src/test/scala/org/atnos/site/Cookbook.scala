package org.atnos.site

object Cookbook extends UserGuidePage {
  def is = "Cookbook".title ^
    s2"""

A collection of examples of how to solve particular problems with Eff

### Partial Interpretation

It's common to want to use different effects in different parts of a program. Some effects, like error handling or
logging, may extend through the whole of our program. However we may to include additional effects,
like state, within one part.

The example below shows how we can do this. Notice how the `State` effect is interpreted via `runState` within the program,
while the `Either` and `Writer` effects are resolved at the top level.

When extending effect stacks, its recommended to put the extended expression in its own method.
Note how type annotation `Fx.prepend[State[Int, ?], R]` defines the extension to the base effect stack type `R`.

`:${
    snippet {
      import cats.data._

      import org.atnos.eff._
      import org.atnos.eff.all._
      import org.atnos.eff.syntax.all._

      object PartialInterpretationExample {

        type Stack = Fx.fx2[Either[String, ?], Writer[String, ?]]

        type Err[R] = Either[String, ?] |= R
        type Log[R] = Writer[String, ?] |= R
        type Count[R] = State[Int, ?] |= R

        println(method[Stack](4).runWriter.runEither.run)

        def method[R: Err : Log](n: Int): Eff[R, Int] = for {
        //this call uses the stack R
          _ <- assertErr(n >= 0, s"$n is negative")

          //the call uses the stack R plus an additional State[Int, ?] which is interpreted here. The other effects remain
          result <- subMethod[Fx.prepend[State[Int, ?], R]].runState(n)

          // pattern matching tuples needs to be done on a following line due to rather obscure limitations of Scala
          // for more details why: http://stackoverflow.com/questions/37982029/how-to-annotate-types-of-expressions-within-a-scala-for-expression
          (_, n2) = result
        } yield n2

        def assertErr[R: Err](expr: Boolean, msg: String): Eff[R, Unit] =
          if (!expr)
            left(msg)
          else
            right(())

        def subMethod[R: Err : Log : Count]: Eff[R, Unit] = for {
          c <- get
          c2 = c * c
          _ <- tell(s"changing the count value to $c2")
          _ <- put(c2)
        } yield (())

      }
    }.eval
  }

"""
}
