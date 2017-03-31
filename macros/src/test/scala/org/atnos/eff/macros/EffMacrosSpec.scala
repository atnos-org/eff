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
 generates a TranslatorFactory sub-trait with boilerplate-free methods $generatesTranslatorFactory
 generates a FunctionK sub-trait with boilerplate-free methods $generatesNaturalTransformationInterpreter
"""
  @eff trait KVStoreDsl {
    type _kvstore[R] = KVStore MemberIn R

    case class GetResult[T](result: Option[T])
    def unpackResult[T](getResult: GetResult[T]): Option[T] = getResult.result

    sealed trait KVStore[+A]

    def put[T : Ordering, R :_kvstore](key: String, value: T): Eff[R, Unit]
    def get[T, R :_kvstore](key: String): Eff[R, GetResult[T]]
    def delete[T, R :_kvstore](key: String): Eff[R, Unit]
    def update[T : Ordering, R :_kvstore](key: String, f: T => T): Eff[R, Unit] =
      for {
        vMaybe <- get[T, R](key)
        _ <- vMaybe.result.map(v => put[T, R](key, f(v))).getOrElse(Eff.pure(()))
      } yield ()
  }

  import KVStoreDsl._
  import org.atnos.eff._

  def program[R :_kvstore]: Eff[R, Option[Int]] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int, R]("wild-cats", _ + 12)
      _ <- put("tame-cats", 5)
      n <- get[Int, R]("wild-cats")
      _ <- delete("tame-cats")
    } yield unpackResult(n)
  lazy val theProgram = program[Fx.fx1[KVStore]]

  def generatesBoilerplate = {
    theProgram should beAnInstanceOf[Eff[Fx1[KVStore],Option[Int]]]
  }

  def generatesSideEffectInterpreter = {

    import org.atnos.eff._, interpret._, syntax.all._
    import scala.collection.mutable._

    val sideEffect = new KVStoreDsl.SideEffect {
      val kvs = Map.empty[String, Any]
      def put[T : Ordering](key: String, value: T): Unit = {
        kvs.put(key, value)
        ()
      }
      def get[T](key: String): GetResult[T] = {
        GetResult(kvs.get(key).asInstanceOf[Option[T]])
      }
      def delete[T](key: String): Unit = {
        kvs.remove(key)
        ()
      }
    }

    // run the program with the unsafe interpreter
    val result = sideEffect.run(theProgram).run
    result ==== Some(14)
  }

  def generatesTranslateInterpreter = {
    import org.atnos.eff._, all._, interpret._, syntax.all._
    import cats.implicits._
    import cats.data._

    type WriterString[A] = Writer[String, A]
    type StateMap[A]     = State[Map[String, Any], A]
    type _writerString[R] = WriterString |= R
    type _stateMap[R]     = StateMap |= R


    val tr = new KVStoreDsl.TranslatorFactory3[ThrowableEither, WriterString, StateMap] {
      def put[T : Ordering, U : _throwableEither : _writerString : _stateMap](key: String, value: T): Eff[U, Unit] = for {
        _ <- tell(s"put($key, $value)").into[U]
        _ <- modify((map: Map[String, Any]) => map.updated(key, value)).into[U]
        r <- fromEither(Either.catchNonFatal(())).into[U]
      } yield r
      def get[T, U : _throwableEither : _writerString : _stateMap](key: String): Eff[U, GetResult[T]] = for {
        _ <- tell(s"get($key)").into[U]
        m <- StateEffect.get[U, Map[String, Any]].into[U]
        r <- fromEither(Either.catchNonFatal(m.get(key).map(_.asInstanceOf[T]))).into[U]
      } yield GetResult(r)
      def delete[T, U : _throwableEither : _writerString : _stateMap](key: String): Eff[U, Unit] = for {
        _ <- tell(s"delete($key)").into[U]
        u <- modify((map: Map[String, Any]) => map - key).into[U]
        r <- fromEither(Either.catchNonFatal(())).into[U]
      } yield r

    }

    // run the program with the safe interpreter
    type Stack = Fx.fx4[KVStore, Throwable Either ?, State[Map[String, Any], ?], Writer[String, ?]]

    // The TranslatorFactory instance contains an implicit class that provides the runKVStore method on the program
    import tr._
    val (result, logs) =
      program[Stack].runKVStore.runEither.evalState(Map.empty[String, Any]).runWriter.run

    result ==== Right(Some(14))
  }

  def generatesTranslatorFactory = {
    import org.atnos.eff._, all._, interpret._, syntax.all._
    import cats.implicits._
    import cats.data._

    type StateMap[A]     = State[Map[String, Any], A]
    type _stateMap[R]    = StateMap |= R

    val tr = new KVStoreDsl.TranslatorFactory1[State[Map[String, Any], ?]] {
      def put[T : Ordering, U : _stateMap](key: String, value: T): Eff[U, Unit] =
        modify((map: Map[String, Any]) => map.updated(key, value))
      def get[T, U : _stateMap](key: String): Eff[U, GetResult[T]] = for {
        m <- StateEffect.get[U, Map[String, Any]]
      } yield GetResult(m.get(key).map(_.asInstanceOf[T]))
      def delete[T, U : _stateMap](key: String): Eff[U, Unit] =
        modify((map: Map[String, Any]) => map - key)
    }

    // run the program with the safe interpreter
    type Stack = Fx.fx2[KVStore, StateMap]

    import tr._
    val result =
      program[Stack].runKVStore.evalState(Map.empty[String, Any]).run

    result ==== Some(14)
  }

  def generatesNaturalTransformationInterpreter = {
    import org.atnos.eff._, all._
    import org.atnos.eff.syntax.all._

    val optionInterp = new KVStoreDsl.FunctionK[Option] {
      def put[T](key: String, value: T)(implicit ordering: Ordering[T]): Option[Unit] = Some(())
      def get[T](key: String): Option[GetResult[T]] = Some(GetResult(None))
      def delete[T](key: String): Option[Unit] = Some(())
    }

    runOption(theProgram.transform(optionInterp)).run ==== Some(None)
  }
}
