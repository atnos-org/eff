package org.atnos.eff.macros

import org.specs2.Specification
import org.atnos.eff._
import cats.syntax.all._
import cats.data._
import cats.{Eval, Traverse}
import ReaderEffect._
import WriterEffect._
import EvalEffect._
import Eff._

class EffMacrosSpec extends Specification { def is = s2"""

 generates boilerplate code for custom effects $generatesBoilerplate

"""

  def generatesBoilerplate = {

    @eff trait KVStoreDsl {
      type _kvstore[R] = KVStore MemberIn R

      sealed trait KVStore[+A]

      def put[T, R :_kvstore](key: String, value: T): Eff[R, Unit]
      def get[T, R :_kvstore](key: String): Eff[R, Option[T]]
      def delete[T, R :_kvstore](key: String): Eff[R, Unit]
      def update[T, R :_kvstore](key: String, f: T => T): Eff[R, Unit] =
        for {
          vMaybe <- get[T, R](key)
          _ <- vMaybe.map(v => put[T, R](key, f(v))).getOrElse(Eff.pure(()))
        } yield ()
    }

    import KVStoreDsl._, ops._
    import org.atnos.eff._

    def program[R :_kvstore]: Eff[R, Option[Int]] =
      for {
        _ <- put("wild-cats", 2)
        _ <- update[Int, R]("wild-cats", _ + 12)
        _ <- put("tame-cats", 5)
        n <- get[Int, R]("wild-cats")
        _ <- delete("tame-cats")
      } yield n

    import org.atnos.eff._, interpret._
    import scala.collection.mutable._

    val sideEffect = new KVStoreDsl.SideEffect {
      val kvs = Map.empty[String, Any]
      def put[T](key: String, value: T): Unit = {
        kvs.put(key, value)
        ()
      }
      def get[T](key: String): Option[T] = {
        kvs.get(key).asInstanceOf[Option[T]]
      }
      def delete[T](key: String): Unit = {
        kvs.remove(key)
        ()
      }
    }

    import org.atnos.eff._, syntax.all._

    // run the program with the unsafe interpreter
    val result = sideEffect.run(program[Fx.fx1[KVStore]]).run
    result ==== Some(14)
  }
}
