package org.atnos.site
package lib

import org.specs2.matcher.ExpectationsDescription._

object MemoEffectPage extends UserGuidePage {
  def is = "Memo".title ^ s2"""

The Memo effect allows the caching of expensive computations. Computations are "stored" with a given key, so that the next
computation with the same key will return the previously computed value. When interpreting those computations a `Cache` must
be provided: ${snippet {

      import cats.Eval
      import cats.implicits._
      import org.atnos.eff._, memo._
      import org.atnos.eff.syntax.memo._
      import org.atnos.eff.syntax.eval._
      import org.atnos.eff.syntax.eff._

      type S = Fx.fx2[Memoized, Eval]

      var i = 0

      def expensive[R: _memo]: Eff[R, Int] =
        memoize("key", { i += 1; 10 * 10 })

      (expensive[S] >> expensive[S]).runMemo(ConcurrentHashMapCache()).runEval.run === 100

      "there is only one invocation" <==> (i === 1)

    }.eval}

There are 2 cache implementations provided in this library to support the Memo effect:

 - `org.atnos.eff.ConcurrentHashMapCache`: backed by a `java.util.concurrent.ConcurrentHashMap` where the keys are hashcodes
 for the keys used to memoize the values. This cache is thread-safe but unbounded so use with care!

 - `org.atnos.eff.ConcurrentWeakIdentityHashMapCache`: backed by a `ConcurrentWeakIdentityHashMap` where the keys are `System.identityHashCode`
   for the keys used to memoize the values. This cache is thread-safe and uses weak references which can be garbage collected when
   necessary.

You can also use other, and better, cache implementations like [Caffeine](https://github.com/ben-manes/caffeine) to get more functionalities
like eviction policies, maximum size and so on. You will need to implement the `Cache` interface for this

```scala
trait Cache {
  def memo[V](key: AnyRef, value: =>V): V
}
```

"""
}
