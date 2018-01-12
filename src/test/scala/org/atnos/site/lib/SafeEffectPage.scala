package org.atnos.site
package lib

object SafeEffectPage extends UserGuidePage { def is = "Safe".title ^ s2"""

The Safe effect is useful to handle resources which must be closed even in the presence of exceptions. The main
operations are

 - `finally` to create an action which must always be executed after another one, even if the first one fails
 - `catchThrowable` to handle a thrown exception
 - `bracket(open)(step)(close)` to open a resource, use it and then close it safely. The `close` part is a "finalizer"
<p/>

Let's see an example for the protection of a resource: ${snippet{
import org.atnos.eff.syntax.all._
import org.atnos.eff._, all._

// let's represent a resource which can be in use
case class Resource(values: List[Int] = (1 to 10).toList, inUse: Boolean = false) {
  def isClosed = !inUse
}

var resource = Resource()

// our stack of effects, with safe evaluation
type S = Fx.fx1[Safe]

def openResource: Eff[S, Resource] =
  protect { resource = resource.copy(inUse = true); resource }

def closeResource(r: Resource): Eff[S, Unit] =
  protect(resource = r.copy(inUse = false))

def useResource(ok: Boolean) = (r: Resource) =>
  protect[S, Int](if (ok) r.values.sum else throw new Exception("boo"))

// this program uses the resource safely even if there is an exception
def program(ok: Boolean): (Throwable Either Int, List[Throwable]) =
  bracket(openResource)(useResource(ok))(closeResource).
    runSafe.run
// 8<--
"Results" +
  showResult1("Without exception", program(ok = true),  "resource is closed", resource.isClosed) +
  showResult1("With exception   ", program(ok = false), "resource is closed", resource.isClosed)

}.eval}

As you can see in the signature of `program` the return value of `runSafe` is `(Throwable Either A, List[Throwable])`.
The first part is the result of your program, which may end with an exception, and the second part is the list of
possible exceptions thrown by the finalizers which can themselves fail.

A simpler version of `bracket` is `finally`.

This example show how to use `finally` but also what happens if a finalizer fails:${snippet{
import org.atnos.eff.syntax.all._
import org.atnos.eff._, all._

// our stack of effects, with safe evaluation
type S = Fx.fx1[Safe]

var sumDone: Boolean = false

def setDone(ok: Boolean): Eff[S, Unit] =
  protect[S, Unit](if (ok) sumDone = true else throw new Exception("failed!!"))

// this program tries to set sumDone to true when the computation is done
def program(ok: Boolean, finalizeOk: Boolean): (Throwable Either Int, List[Throwable]) =
  (protect[S, Int](if (ok) (1 to 10).sum else throw new Exception("boo")) `finally` setDone(finalizeOk)).
    runSafe.run

// 8<--
"Results" +
  showResult2("Computation ok, finalizer ok", program(ok = true, finalizeOk = true)  ) +
  showResult2("Computation ok, finalizer ko", program(ok = true, finalizeOk = false) ) +
  showResult2("Computation ko, finalizer ok", program(ok = false, finalizeOk = true) ) +
  showResult2("Computation ko, finalizer ko", program(ok = false, finalizeOk = false))
}.eval}

Finally (no pun intended!) note that you can use `execSafe` if you are not interested in the result of the finalizers.

"""

  import cats.implicits._

  def showResult1(message: String, result: (Throwable Either Int, List[Throwable]), resourceMessage: String, closed: Boolean) =
    result match {
      case (r, ls) =>
        s"""|
            |$message: ${r.leftMap(_.getMessage)}, finalizers exceptions: ${if (ls.isEmpty) "no exceptions" else ls.map(_.getMessage).toString},\t$resourceMessage: $closed""".stripMargin
    }

  def showResult2(message: String, result: (Throwable Either Int, List[Throwable])) =
    result match {
      case (r, ls) =>
        s"""|
          |$message: ${r.leftMap(_.getMessage)}, finalizers exceptions: ${if (ls.isEmpty) "no exceptions" else ls.map(_.getMessage).toString}""".stripMargin
    }

}
