// 8<---
package org.atnos.site.snippets.tutorial
import AdtSnippet._
import org.atnos.eff._

trait AdtCreationSnippet {
// 8<---

import cats.implicits._

// T |= R is an alias for MemberIn[T, R]
// stating that effects of type T[_] can be injected in the effect stack R
// It is also equivalent to MemberIn[KVStore, R]
type _kvstore[R] = KVStore |= R

/** store returns nothing (i.e. Unit) */
def store[T, R :_kvstore](key: String, value: T): Eff[R, Unit] =
  Eff.send[KVStore, R, Unit](Put(key, value))

/** find returns a T value if the key exists */
def find[T, R :_kvstore](key: String): Eff[R, Option[T]] =
  Eff.send[KVStore, R, Option[T]](Get(key))

/** delete returns nothing (i.e. Unit) */
def delete[T, R :_kvstore](key: String): Eff[R, Unit] =
  Eff.send(Delete(key))

/** update composes get and put, and returns nothing. */
def update[T, R :_kvstore](key: String, f: T => T): Eff[R, Unit] =
  for {
    ot <- find[T, R](key)
    _  <- ot.map(t => store[T, R](key, f(t))).getOrElse(Eff.pure(()))
  } yield ()

  // 8<---
}

object AdtCreationSnippet extends AdtCreationSnippet
