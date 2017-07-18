package org.atnos.eff.macros

import org.specs2.Specification
import org.atnos.eff._

class EffMacrosSpec extends Specification { def is = s2"""

 The @eff macro annotation must
   generate boilerplate code for custom effects                         $generatesBoilerplate
   generate a SideEffect sub-class with boilerplate-free methods        $generatesSideEffectInterpreter
   generate a Translate sub-trait with boilerplate-free methods         $generatesTranslateInterpreter
   generate a TranslatorFactory sub-trait with boilerplate-free methods $generatesTranslatorFactory
   generate a FunctionK sub-trait with boilerplate-free methods         $generatesNaturalTransformationInterpreter
   override an existing companion object                                $overrideExistingCompanion

"""

  @eff trait KVStoreDsl {
    type _kvstore[R] = KVStore |= R

    case class GetAllResult[T](result: List[T])
    case class GetResult[T](result: Option[T])

    sealed trait KVStore[+A]

    def put[T : Ordering, R :_kvstore](key: String, value: T): Eff[R, Unit]
    def getAll[T, R :_kvstore]: Eff[R, GetAllResult[T]]
    def get[T, R :_kvstore](key: String): Eff[R, GetResult[T]]
    def delete[T, R :_kvstore](key: String): Eff[R, Unit]
    def update[T : Ordering, R :_kvstore](key: String, f: T => T): Eff[R, Unit] =
      for {
        vMaybe <- get[T, R](key)
        _ <- vMaybe.result.map(v => put[T, R](key, f(v))).getOrElse(Eff.pure(()))
      } yield ()
  }

  object KVStoreDsl {
    val someConstant = 1
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
    } yield n.result
  lazy val theProgram = program[Fx.fx1[KVStore]]

  def generatesBoilerplate = {
    theProgram should beAnInstanceOf[Eff[Fx1[KVStore],Option[Int]]]
  }

  def generatesSideEffectInterpreter = {

    import org.atnos.eff._, syntax.all._
    import scala.collection.mutable._

    val sideEffect = new KVStoreDsl.SideEffect {
      val kvs = Map.empty[String, Any]
      def put[T : Ordering](key: String, value: T): Unit = {
        kvs.put(key, value)
        ()
      }
      def getAll[T]: GetAllResult[T] =
        GetAllResult(kvs.values.toList.asInstanceOf[List[T]])

      def get[T](key: String): GetResult[T] =
        GetResult(kvs.get(key).asInstanceOf[Option[T]])

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

        def getAll[T]: Eff[U, GetAllResult[T]] = for {
          _ <- tell(s"get all")
          m <- StateEffect.get[U, Map[String, Any]]
          r <- fromEither(Either.catchNonFatal(m.values.toList.map(_.asInstanceOf[T])))
        } yield GetAllResult(r)


        def get[T](key: String): Eff[U, GetResult[T]] = for {
          _ <- tell(s"get($key)")
          m <- StateEffect.get[U, Map[String, Any]]
          r <- fromEither(Either.catchNonFatal(m.get(key).map(_.asInstanceOf[T])))
        } yield GetResult(r)

        def delete[T](key: String): Eff[U, Unit] = for {
          _ <- tell(s"delete($key)")
          u <- modify((map: Map[String, Any]) => map - key)
          r <- fromEither(Either.catchNonFatal(()))
        } yield r

      }
      translate(effects)(tr)
    }

    // run the program with the safe interpreter
    type Stack = Fx.fx4[KVStore, Throwable Either ?, State[Map[String, Any], ?], Writer[String, ?]]

    val (result, logs) =
      runKVStore(program[Stack]).runEither.evalState(Map.empty[String, Any]).runWriter.run

    result ==== Right(Some(14))
  }

  def generatesTranslatorFactory = {
    import org.atnos.eff._, all._, syntax.all._
    import cats.implicits._
    import cats.data._

    type WriterString[A] = Writer[String, A]
    type StateMap[A]     = State[Map[String, Any], A]
    type _writerString[R] = WriterString |= R
    type _stateMap[R]     = StateMap |= R


    val tr = new KVStoreDsl.TranslatorFactory3[ThrowableEither, WriterString, StateMap] {
      def put[T : Ordering, U :_throwableEither :_writerString :_stateMap](key: String, value: T): Eff[U, Unit] = for {
        _ <- tell(s"put($key, $value)").into[U]
        _ <- modify((map: Map[String, Any]) => map.updated(key, value)).into[U]
        r <- fromEither(Either.catchNonFatal(())).into[U]
      } yield r

      def getAll[T, U :_throwableEither :_writerString :_stateMap]: Eff[U, GetAllResult[T]] = for {
        _ <- tell(s"get all").into[U]
        m <- StateEffect.get[U, Map[String, Any]].into[U]
        r <- fromEither(Either.catchNonFatal(m.values.toList.map(_.asInstanceOf[T]))).into[U]
      } yield GetAllResult(r)

      def get[T, U :_throwableEither :_writerString :_stateMap](key: String): Eff[U, GetResult[T]] = for {
        _ <- tell(s"get($key)").into[U]
        m <- StateEffect.get[U, Map[String, Any]].into[U]
        r <- fromEither(Either.catchNonFatal(m.get(key).map(_.asInstanceOf[T]))).into[U]
      } yield GetResult(r)

      def delete[T, U :_throwableEither :_writerString :_stateMap](key: String): Eff[U, Unit] = for {
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

  def generatesNaturalTransformationInterpreter = {
    import org.atnos.eff._, all._
    import org.atnos.eff.syntax.all._

    val optionInterp = new KVStoreDsl.FunctionK[Option] {
      def put[T](key: String, value: T)(implicit ordering: Ordering[T]): Option[Unit] = Some(())
      def get[T](key: String): Option[GetResult[T]] = Some(GetResult(None))
      def getAll[T]: Option[GetAllResult[T]] = Some(GetAllResult(Nil))
      def delete[T](key: String): Option[Unit] = Some(())
    }

    runOption(theProgram.transform(optionInterp)).run ==== Some(None)
  }

  def overrideExistingCompanion = {
    KVStoreDsl.someConstant ==== 1
  }

}
