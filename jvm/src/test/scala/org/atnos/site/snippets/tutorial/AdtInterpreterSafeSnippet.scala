// 8<---
package org.atnos.site.snippets.tutorial

import AdtSnippet._
import AdtCreationSnippet._
import cats.arrow.NaturalTransformation

trait AdtInterpreterSafeSnippet {
// 8<---

import org.atnos.eff._, all._, interpret._, syntax._
import cats.implicits._
import cats.data._

/**
 * Safe interpreter for KVStore effects
 *
 * It uses the following effects:
 *
 *  - Writer to create log statements
 *  - State to update a key-value Map
 *  - Xor to raise errors if the type of an object in the map is not of the expected type
 *
 *  The resulting effect stack is U which is R without the KVStore effects
 *
 *  Note that we just require the Throwable, Writer and State effects to
 *  be able to be created in the stack U with |= (and not <=)
 */
def runKVStore[R, U, A](effects: Eff[R, A])
  (implicit
   m: Member.Aux[KVStore, R, U],
   x: Throwable Xor ? |= U,
   w: Writer[String, ?] |= U,
   s: State[Map[String, Any], ?] |= U
  ): Eff[U, A] = {

  val translation = new Translate[KVStore, U] {
    def apply[X](kv: KVStore[X]): Eff[U, X] =
      kv match {
        case Put(key, value) =>
          for {
            _ <- tell(s"put($key, $value)")
            _ <- modify((map: Map[String, Any]) => map.updated(key, value))
            r <- fromXor(Xor.catchNonFatal(().asInstanceOf[X]))
          } yield r

        case Get(key) =>
          for {
            _ <- tell(s"get($key)")
            m <- get[U, Map[String, Any]]
            r <- fromXor(Xor.catchNonFatal(m.get(key).asInstanceOf[X]))
          } yield r

        case Delete(key) =>
          for {
            _ <- tell(s"delete($key)")
            u <- modify((map: Map[String, Any]) => map - key)
            r <- fromXor(Xor.catchNonFatal(().asInstanceOf[X]))
          } yield r
      }
  }

  translate(effects)(translation)
}

// 8<---
}

object AdtInterpreterSafeSnippet extends AdtInterpreterSafeSnippet
