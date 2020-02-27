// 8<---
package org.atnos.site.snippets.tutorial

import AdtSnippet._

trait AdtInterpreterSafeSnippet {
// 8<---

import org.atnos.eff._
import org.atnos.eff.either._
import org.atnos.eff.writer._
import org.atnos.eff.state._
import org.atnos.eff.interpret._
import cats.implicits._
import cats.data._

type _writerString[R] = Writer[String, *] |= R
type _stateMap[R]     = State[Map[String, Any], *] |= R

/**
 * Safe interpreter for KVStore effects
 *
 * It uses the following effects:
 *
 *  - Writer to create log statements
 *  - State to update a key-value Map
 *  - Either to raise errors if the type of an object in the map is not of the expected type
 *
 *  The resulting effect stack is U which is R without the KVStore effects
 *
 *  Note that we just require the Throwable, Writer and State effects to
 *  be able to be created in the stack U
 *
 * This interpreter uses the org.atnos.eff.interpreter.translate method
 * translating one effect of the stack to other effects in the same stack
 *
 *
 * NOTE:
 * - It is really important for type inference that the effects for U are listed after those for R!
 *
 * Implicit member definitions will NOT be found with the following definition:
 *
 * def runKVStore[R, U :_throwableEither :_writerString :_stateMap, A](effects: Eff[R, A]) (
 *   implicit m: Member.Aux[KVStore, R, U]): Eff[U, A] = {
 *
 */
def runKVStore[R, U, A](effects: Eff[R, A])
  (implicit m: Member.Aux[KVStore, R, U],
            throwable:_throwableEither[U],
            writer:_writerString[U],
            state:_stateMap[U]): Eff[U, A] = {

  translate(effects)(new Translate[KVStore, U] {
    def apply[X](kv: KVStore[X]): Eff[U, X] =
      kv match {
        case Put(key, value) =>
          for {
            _ <- tell(s"put($key, $value)")
            _ <- modify((map: Map[String, Any]) => map.updated(key, value))
            r <- fromEither(Either.catchNonFatal(().asInstanceOf[X]))
          } yield r

        case Get(key) =>
          for {
            _ <- tell(s"get($key)")
            m <- get[U, Map[String, Any]]
            r <- fromEither(Either.catchNonFatal(m.get(key).asInstanceOf[X]))
          } yield r

        case Delete(key) =>
          for {
            _ <- tell(s"delete($key)")
            u <- modify((map: Map[String, Any]) => map - key)
            r <- fromEither(Either.catchNonFatal(().asInstanceOf[X]))
          } yield r
      }
  })
}

// 8<---
}

object AdtInterpreterSafeSnippet extends AdtInterpreterSafeSnippet
