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
 generates a SideEffect sub-class with boilerplate-free methods $generatesSideEffectInterpreter
 generates a Translate sub-trait with boilerplate-free methods $generatesTranslateInterpreter
 generates a FunctionK sub-trait with boilerplate-free methods $generatesNaturalTransformationInterpreter
"""
  @eff trait KVStoreDsl {
    type _kvstore[R] = KVStore MemberIn R

    sealed trait KVStore[+A]

    def put[T : Ordering, R :_kvstore](key: String, value: T): Eff[R, Unit]
    def get[T, R :_kvstore](key: String): Eff[R, Option[T]]
    def delete[T, R :_kvstore](key: String): Eff[R, Unit]
    def update[T : Ordering, R :_kvstore](key: String, f: T => T): Eff[R, Unit] =
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
  lazy val theProgram = program[Fx.fx1[KVStore]]

  def generatesBoilerplate = {
    theProgram should beAnInstanceOf[Eff[Fx1[KVStore],Option[Int]]]
  }

  def generatesSideEffectInterpreter = {

    import org.atnos.eff._, interpret._
    import scala.collection.mutable._

    val sideEffect = new KVStoreDsl.SideEffect {
      val kvs = Map.empty[String, Any]
      def put[T : Ordering](key: String, value: T): Unit = {
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
    val result = sideEffect.run(theProgram).run
    result ==== Some(14)
  }

  def generatesTranslateInterpreter = {
    import org.atnos.eff._, all._, interpret._
    import cats.implicits._
    import cats.data._
    import KVStore._

    type _writerString[R] = Writer[String, ?] |= R
    type _stateMap[R]     = State[Map[String, Any], ?] |= R

    def runKVStore[R, U, A](effects: Eff[R, A])
      (implicit m: Member.Aux[KVStore, R, U],
        throwable:_throwableEither[U],
        writer:_writerString[U],
        state:_stateMap[U]): Eff[U, A] = {

      val tr = new KVStoreDsl.Translate[R, U] {
        def put[T](key: String, value: T)(implicit ordering: Ordering[T]): Eff[U, Unit] = for {
          _ <- tell(s"put($key, $value)")
          _ <- modify((map: Map[String, Any]) => map.updated(key, value))
          r <- fromEither(Either.catchNonFatal(()))
        } yield r
        def get[T](key: String): Eff[U, Option[T]] = for {
          _ <- tell(s"get($key)")
          m <- StateEffect.get[U, Map[String, Any]]
          r <- fromEither(Either.catchNonFatal(m.get(key).map(_.asInstanceOf[T])))
        } yield r
        def delete[T](key: String): Eff[U, Unit] = for {
          _ <- tell(s"delete($key)")
          u <- modify((map: Map[String, Any]) => map - key)
          r <- fromEither(Either.catchNonFatal(()))
        } yield r

      }
      translate(effects)(tr)
    }
    import org.atnos.eff._, syntax.all._
    import cats._, data._

    // run the program with the safe interpreter
    type Stack = Fx.fx4[KVStore, Throwable Either ?, State[Map[String, Any], ?], Writer[String, ?]]

    val (result, logs) =
      runKVStore(program[Stack]).runEither.evalState(Map.empty[String, Any]).runWriter.run

    result ==== Right(Some(14))
  }

  def generatesNaturalTransformationInterpreter = {
    import KVStore._
    import org.atnos.eff._, all._
    import org.atnos.eff.syntax.all._

    val optionInterp = new KVStoreDsl.FunctionK[Option] {
      def put[T](key: String, value: T)(implicit ordering: Ordering[T]): Option[Unit] = Some(())
      def get[T](key: String): Option[Option[T]] = Some(None)
      def delete[T](key: String): Option[Unit] = Some(())
    }

    runOption(theProgram.transform(optionInterp)).run ==== Some(None)
  }
}
