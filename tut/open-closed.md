## Open vs closed

There are 2 ways to create effectful computations for a given effect `M`. 
The first one is to specify the stack which contains the effect:

```scala
import org.specs2.control.eff._
import Eff._
import Effects._
import StateEffect._
import WriterEffect._
import cats.syntax.all._
import cats.state._
import cats.data._

type StateInt[A] = State[Int, A]
type WriterString[A] = Writer[String, A]

type S = StateInt |: WriterString |: NoEffect

implicit def StateIntMember: Member[StateInt, S] = Member.MemberNatIsMember
implicit def WriterStringMember: Member[WriterString, S] = Member.MemberNatIsMember

 def putAndTell(i: Int): Eff[S, Int] = 
   for {
     _ <- put(i)
     _ <- tell("stored "+i)
   } yield i
```

This has several advantages:

 - this specifies in which order the effects are to be interpreted. This is very important because the 
   result of running `StateInt` followed by `WriterString` is of type `((S, A), List[W])` whereas running 
   `WriterString` followed by `StateInt` returns a value of type `(S, (A, List[W]))`
    
 - type inference works well and there is no need to annotate the `put` or `tell` methods.   

On the other hand we lose some flexibility:

 - adding another effect requires to transform the definition of the effect stack `S`
 - the `putAndTell` method cannot be used in 2 different effects stacks having the `StateInt` and `WriterString` effects
 
The remedy is to use the `Member` typeclass to create an **open** union of effects: 

```scala
scala> import Member.<=
import Member.$less$eq

scala> def putAndTell[R](i: Int)(implicit s: StateInt <= R, w: WriterString <= R): Eff[R, Int] = 
     |   for {
     |     _ <- put[R, Int](i)
     |     _ <- tell[R, String]("stored "+i)
     |   } yield i
putAndTell: [R](i: Int)(implicit s: org.specs2.control.eff.Member.<=[StateInt,R], implicit w: org.specs2.control.eff.Member.<=[WriterString,R])org.specs2.control.eff.Eff[R,Int]
```
 
Here we just declare that the `putAndTell` method can be used with any effect stack `R` if the `StateInt` and `WriterString`
effects are members of that stack. However we have to add some type annotations to guide the compiler. 
