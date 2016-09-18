package org.atnos.site

import org.specs2.execute.Snippet

object ApplicativeEvaluation extends UserGuidePage { def is = "Applicative".title ^ s2"""

### Concurrent evaluation

The default interpretation of `Eff` values is "monadic" meaning that effectful values are being evaluated in order. This
  becomes clear when traversing a list of values with the `FutureEffect`:${snippet{
import org.atnos.eff._, all._, syntax.all._
import cats.Eval
import cats.data.Writer
import cats.syntax.traverse._
import cats.instances.list._
import scala.concurrent._, duration._, ExecutionContext.Implicits.global

type WriterString[A] = Writer[String, A]
type _writerString[R] = WriterString |= R

type S = Fx.fx3[Eval, Future, WriterString]

def execute[E :_eval :_writerString :_future](i: Int): Eff[E, Int] =
  for {
    i1 <- delay(i)
    i2 <- async(i1)
    _  <- tell(i2.toString)
  } yield i2

val action: Eff[S, List[Int]] =
  List(1000, 500, 50).traverse(execute[S])

action.runEval.awaitFuture(2.seconds).runWriterLog.run

}.eval}


We can however run all those computations concurrently using the applicative execution for `Eff`:${snippet{
 // 8<--
import org.atnos.eff._, all._, syntax.all._
import cats.Eval
import cats.data.Writer
import cats.instances.list._
import scala.concurrent._, duration._, ExecutionContext.Implicits.global

type WriterString[A] = Writer[String, A]
type _writerString[R] = WriterString |= R

type S = Fx.fx3[Eval, Future, WriterString]

def execute[E :_eval :_writerString :_future](i: Int): Eff[E, Int] =
  for {
    i1 <- delay(i)
    i2 <- async(i1)
    _  <- tell(i2.toString)
  } yield i2
// 8<--

val action: Eff[S, List[Int]] =
  List(1000, 500, 50).traverseA(execute[S])

action.runEval.awaitFuture(2.seconds).runWriterLog.run
}.eval}

This uses now `traverseA` (instead of `traverse`) to do an applicative traversal and execute futures concurrently and
the fastest actions finish first.

### Batching

Another advantage of applicative effects is that we can intercept them individual requests and "batch" them into one single
request. For example: ${snippet {
import org.atnos.eff._, all._, syntax.all._
import cats._
import cats.implicits._

// An effect to get users from a database
// calls can be individual or batched
case class User(i: Int)
sealed trait UserDsl[+A]

case class GetUser(i: Int) extends UserDsl[User]
case class GetUsers(is: List[Int]) extends UserDsl[List[User]]
type _userDsl[R] = UserDsl /= R

def getUser[R :_userDsl](i: Int): Eff[R, User] =
  send[UserDsl, R, User](GetUser(i))

def getUsers[R :_userDsl](is: List[Int]): Eff[R, List[User]] =
  send[UserDsl, R, List[User]](GetUsers(is))

/*p
Let's create an interpreter for this DSL:
*/
// the real method calls to a webservice
def getWebUser(i: Int): User = User(i)
def getWebUsers(is: List[Int]): List[User] = is.map(i => User(i))

// the interpreter simply calls the webservice
// and return a trace of the executed call
def runDsl[A](eff: Eff[Fx1[UserDsl], A]): (A, Vector[String]) = {
  def go(e: Eff[Fx1[UserDsl], A], trace: Vector[String]): (A, Vector[String]) =
    e match {
      case Pure(a) => (a, trace)
      case Impure(Union1(GetUser(i)), c)   => go(c(getWebUser(i)), trace :+ "getWebUser")
      case Impure(Union1(GetUsers(is)), c) => go(c(getWebUsers(is)), trace :+ "getWebUsers")
      case ap @ ImpureAp(u, m)             => go(ap.toMonadic, trace)
  }
  go(eff, Vector())
}

/*p
We can also optimise a `UserDsl` program by providing a `Semigroup` instance describing how to "batch"
2 calls into 1:
*/
implicit def SemigroupUserDsl: Semigroup[UserDsl[_]] = new Semigroup[UserDsl[_]] {
  def combine(tx: UserDsl[_], ty: UserDsl[_]): UserDsl[_] =
    (tx, ty) match {
      case (GetUser(i),   GetUser(j))   => GetUsers(List(i, j))
      case (GetUser(i),   GetUsers(is)) => GetUsers(i :: is)
      case (GetUsers(is), GetUser(i))   => GetUsers(is :+ i)
      case (GetUsers(is), GetUsers(js)) => GetUsers(is ++ js)
    }
}

/*p
Now let's create a program using the `User` DSL with applicative calls
 which can be optimised:
*/
def program[R :_userDsl]: Eff[R, List[User]] =
  Eff.traverseA(List(1, 2, 3))(i => getUser(i))

/*p
And its optimised version:
*/
def optimised[R :_userDsl]: Eff[R, List[User]] =
  optimiseBatching(program)

/*p
Running the optimised and non-optimised version of the program must yield the same results:
*/
// 8<--
def show(l1: (List[User], Vector[String]), l2: (List[User], Vector[String])) =
  s"""
    |original:  ${l1._1.mkString(", ")}
    |  trace: ${l1._2.mkString(", ")}
    |
    |optimised: ${l2._1.mkString(", ")}
    |  trace: ${l2._2.mkString(", ")}""".stripMargin
// 8<--

show(runDsl(program[Fx1[UserDsl]]), runDsl(optimised[Fx1[UserDsl]]))
}.noPrompt.eval}

***NOTE***

The `optimiseBatching` combinator only works for *some* Semigroups, namely the ones where the aggregation of effects
return an effect for a list of values. It is unsafe to use with other Semigroups! See the `org.atnos.eff.Optimise` for
more information.

"""

}

