// 8<---
package org.atnos.site.snippets.tutorial

import AdtSnippet._

trait AdtInterpreterSnippet {
// 8<---
  import org.atnos.eff._
  import interpret._
  import cats.Traverse
  import cats.syntax.all._
  import scala.collection.mutable._

  /**
 * Unsafe interpreter for KVStore effects
 *
 * the program will crash if a type is incorrectly specified.
 *
 * The interpreter requires the KVStore effect to be a Member of R (with <=)
 * Meaning that we can statically know the resulting type once we have removed
 * KVStore from R, and this type is m.Out.
 *
 * The interpreter uses the `interpretUnsafe` method from `org.atnos.eff.Interpreter` to implement a
 * stack-safe interpretation of effects as a side-effect.
 *
 * `interpretUnsafe` needs the definition of a side-effect where
 * we get each `KVStore[X]` effect, run side-effects and return a value `X`.
 *
 * The resulting effect stack is m.Out which is R without the KVStore effects
 *
 */
  def runKVStoreUnsafe[R, A](effects: Eff[R, A])(implicit m: KVStore <= R): Eff[m.Out, A] = {
    // a very simple (and imprecise) key-value store
    val kvs = Map.empty[String, Any]

    val sideEffect = new SideEffect[KVStore] {
      def apply[X](kv: KVStore[X]): X =
        kv match {
          case Put(key, value) =>
            println(s"put($key, $value)")
            kvs.put(key, value)
            ()

          case Get(key) =>
            println(s"get($key)")
            kvs.get(key).asInstanceOf[X]

          case Delete(key) =>
            println(s"delete($key)")
            kvs.remove(key)
            ()
        }

      def applicative[X, Tr[_]: Traverse](ms: Tr[KVStore[X]]): Tr[X] =
        ms.map(apply)
    }
    interpretUnsafe(effects)(sideEffect)(m)

  }

  // 8<---
}

object AdtInterpreterSnippet extends AdtInterpreterSnippet
