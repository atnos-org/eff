package org.atnos.eff.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class eff extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro EffMacros.impl
}

class EffMacros(val c: blackbox.Context) {
  import c.universe._

  def abort(msg: String) = c.abort(c.enclosingPosition, msg)

  // https://issues.scala-lang.org/browse/SI-8771
  def fixSI88771(paramss: Any) = paramss.asInstanceOf[List[List[ValDef]]].map(_.map(_.duplicate))

  def replaceContainerType(tree: Trees#Tree, newType: TypeName): AppliedTypeTree = tree match {
    case AppliedTypeTree(_, inner) => AppliedTypeTree(Ident(newType), inner)
    case other => abort(s"Not an AppliedTypeTree: ${showRaw(other)}")
  }

  def adt(name: Name) = q"${TermName(name.toString.capitalize)}"

  type Paramss = List[List[ValDef]]
  def paramssToArgs(paramss: Paramss): List[List[TermName]] =
    paramss.filter(_.nonEmpty).map(_.collect { case t@ValDef(mods, name, _, _) => name })

  // remove () if no args
  def methodCallFmt(method: c.universe.Tree, args: Seq[Seq[TermName]]) = if (args.flatten.isEmpty) method else q"$method(...$args)"

  def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val trees = annottees.map(_.tree).toList

    trees.headOption match {
      case Some(q"$_ trait ${tpname:TypeName}[..$_] extends { ..$earlydefns } with ..$parents { $self => ..$stats }") =>
        val (typeAlias: TypeDef, freeSType) =
          stats.collectFirst {
            case typeDef @ q"type $_[..$_] = MemberIn[${Ident(s)}, $_]" => (typeDef, s)
            case typeDef @ q"type $_[..$_] = |=[${Ident(s)}, $_]" => (typeDef, s)
            case typeDef @ q"type $_[..$_] = /=[${Ident(s)}, $_]" => (typeDef, s)
            case typeDef @ q"type $_[..$_] = <=[${Ident(s)}, $_]" => (typeDef, s)
          }.getOrElse(abort(s"$tpname needs to define a type alias for MemberIn[S, A]"))

        val sealedTrait: ClassDef = stats.collectFirst {
          case cd @ q"sealed trait $name[..$_]" if name == freeSType => cd.asInstanceOf[ClassDef]
        }.getOrElse(abort(s"$tpname needs to define a sealed trait $freeSType[A]"))

        case class EffType(effectType: Tree, returnTypeTree: Tree)

        object ExpectedReturnType {
          def unapply(rt: Tree): Option[EffType] = rt match {
            case AppliedTypeTree(Ident(name), List(effectType, returnType))  if name.toString == "Eff" =>
              Some(EffType(effectType, returnType))
            case _ => None
          }
        }
        def isReturnTypeOfTypeAlias(rt: Tree): Boolean = rt match {
          case ExpectedReturnType(_) => true
          case _ => false
        }

        def findImplicitBracket(paramss: List[List[ValDef]]): Option[Int] = {
          paramss.zipWithIndex.find { case (params, idx) => params.exists(_.mods.hasFlag(Flag.IMPLICIT))}.map(_._2)
        }
        def addImplicits(newImplicits: List[ValDef], paramss: List[List[ValDef]]) = {
          findImplicitBracket(paramss).map { idx =>
            paramss.updated(idx, paramss(idx) ++ newImplicits)
          }.getOrElse {
            paramss :+ newImplicits
          }
        }

        def nonStackParams(paramss: List[List[ValDef]]) = {
          paramss.map { params =>
            params.filter {
              case ValDef(_, _, AppliedTypeTree(Ident(name), _), _) => name != typeAlias.name
              case _ => true
            }
          }.filterNot(_.isEmpty)
        }

        // check some constraints that will result in a compiler error
        stats.foreach {
          case x:ValOrDefDef if x.mods.hasFlag(Flag.PRIVATE|Flag.PROTECTED) => c.abort(x.pos, "try using access modifier: package-private")
          case v @ ValDef(_, _, rt: TypeTree, _)       => c.abort(v.pos, s"Define the return type for:") // requires explicit return type
          case d @ DefDef(_, _, _, _, rt: TypeTree, _) => c.abort(d.pos, s"Define the return type for:") // requires explicit return type
          case v @ ValDef(mods, _, rt, _) if mods.hasFlag(Flag.MUTABLE) => c.abort(v.pos, s"var is not allow in @eff trait $tpname")
          case v @ ValDef(_, _, rt, EmptyTree)  =>
            if (!isReturnTypeOfTypeAlias(rt)) c.abort(v.pos, s"Abstract val needs to have return type ${typeAlias.name}[...], otherwise, make it non-abstract.")
          case d @ DefDef(_, _, _, _, rt, EmptyTree) =>
            if (!isReturnTypeOfTypeAlias(rt)) c.abort(d.pos, s"Abstract def needs to have return type ${typeAlias.name}[...], otherwise, make it non-abstract.")
          case _ => // no issue
        }

        val absValsDefsOps: Seq[ValOrDefDef] = stats.collect {
          case m @ DefDef(_, _, _, _, ExpectedReturnType(_), EmptyTree) => m
        }

        // --------------------------------------------------------------------------------
        // vals name with op(s) means having return type matching the defined typeAlias.
        // And vise versa, nonOp(s) means having different return type than the typeAlias.
        // --------------------------------------------------------------------------------
        val liftedOps:Seq[DefDef] = absValsDefsOps.map {
          case DefDef(_, name, tparams, paramss, ExpectedReturnType(EffType(effectType, returnType)), _) =>
            val args = paramssToArgs(nonStackParams(paramss)).flatten
            val rhs = q"Eff.send[${sealedTrait.name}, $effectType, $returnType](${adt(name)}(..$args))"
            val params = (if (paramss.isEmpty) List.empty else paramss)
            q"def $name[..$tparams](...$params): Eff[$effectType, $returnType] = $rhs".asInstanceOf[DefDef]
        }

        val injectOpsObj = liftedOps

        val genCaseClassesAndObjADT =
          absValsDefsOps.collect {
            case q"..$_ def $tname[..$tparams](...$paramss): ${AppliedTypeTree(_, returnType)} = $expr" =>
              val fixedParams = fixSI88771(paramss)

              val implicits = fixedParams match {
                // this should not happen, there should be implicit parameters
                case Nil           => Nil
                case ps :: is :: _ => is
                case is :: _       => is
              }

              val implicitParams = implicits.collect {
                case ValDef(_, _, AppliedTypeTree(Ident(name), Seq(Ident(stackTypeName))), _) if name == typeAlias.name => stackTypeName
              }
              val nonStackReturnTypes = returnType.filter {
                case Ident(name) if implicitParams.contains(name) => false
                case _ => true
              }
              val nonStackTypeParams = tparams.filter {
                case TypeDef(_, name, _, _) if implicitParams.contains(name) => false
                case _ => true
              }
              q"case class ${TypeName(tname.toString.capitalize)}[..$nonStackTypeParams](..${nonStackParams(fixedParams).flatten}) extends ${sealedTrait.name}[..$nonStackReturnTypes]"
          }

        val sideEffectTrait = {
          val methodsToBeImpl: Seq[DefDef] = absValsDefsOps.map {
            case q"..$mods def $name[..$tparams](...$paramss): Eff[$_, $returnType]" =>
              DefDef(mods, name, tparams.dropRight(1), nonStackParams(paramss), returnType, EmptyTree)
          }

          q"""
              trait SideEffect extends org.atnos.eff.SideEffect[${sealedTrait.name}] {
                def apply[A](fa: ${sealedTrait.name}[A]): A = fa match {
                  case ..${absValsDefsOps.map {
                    case DefDef(_, name, _, paramss, rt, _) =>
                      val binds = nonStackParams(paramss).flatMap(_.collect { case t:ValDef => Bind (t.name, Ident(termNames.WILDCARD))})
                      val args = nonStackParams(paramss).map(_.collect { case t:ValDef => Ident(t.name.toTermName) })
                      val rhs = if (args.isEmpty) q"$name" else q"$name(...${args})"
                      cq"${adt(name)}(..$binds) => $rhs"
                    case ValDef(_, name, _, _) =>
                      cq"${adt(name)} => $name"
                  }}
                }
                def applicative[X, Tr[_]](ms: Tr[${sealedTrait.name}[X]])(implicit traverse: cats.Traverse[Tr]): Tr[X] =
                  traverse.map(ms)(apply)
                def run[R, A](effects: Eff[R, A])(implicit m: ${sealedTrait.name} <= R): Eff[m.Out, A] =
                  org.atnos.eff.interpret.interpretUnsafe(effects)(this)(m)
                ..$methodsToBeImpl
              }
        """
        }

        val translateTrait =  {
          val methodsToBeImpl: Seq[DefDef] = absValsDefsOps.map {
            case q"..$mods def $name[..$tparams](...$paramss): Eff[$_, $returnType]" =>
              DefDef(mods, name, tparams.dropRight(1), nonStackParams(paramss), tq"Eff[U, $returnType]", EmptyTree)
          }

          q"""
              trait Translate[R, U] extends org.atnos.eff.Translate[${sealedTrait.name}, U] {
                def apply[X](fa: ${sealedTrait.name}[X]): Eff[U, X] = fa match {
                  case ..${absValsDefsOps.map {
                    case DefDef(_, name, _, paramss, rt, _) =>
                      val binds = nonStackParams(paramss).flatMap(_.collect { case t:ValDef => Bind (t.name, Ident(termNames.WILDCARD))})
                      val args = nonStackParams(paramss).map(_.collect { case t:ValDef => Ident(t.name.toTermName) })
                      val rhs = if (args.isEmpty) q"$name" else q"$name(...${args})"
                      cq"${adt(name)}(..$binds) => $rhs.asInstanceOf[Eff[U, X]]"
                    case ValDef(_, name, _, _) =>
                      cq"${adt(name)} => $name.asInstanceOf[Eff[U, X]]"
                  }}
                }
                ..$methodsToBeImpl
              }
        """
        }

        val translatorFactoryTraits = (1 until 10).map  { numOutEffects =>
          def replacementParamName(i: Int) = TermName(s"__replacement$i")
          val factoryTrait = TypeName(s"TranslatorFactory$numOutEffects")
          val effectImplicitParams = (1 to numOutEffects).map(i => ValDef(Modifiers(Flag.IMPLICIT), replacementParamName(i), tq"${TypeName(s"R$i")} MemberIn U", EmptyTree)).toList
          val effectImplicitArgs = (1 to numOutEffects).map(replacementParamName)
          val typeU = TypeDef(Modifiers(Flag.PARAM), TypeName("U"), List(), TypeBoundsTree(tq"scala.Nothing", tq"scala.Any"))

          val methodsToBeImpl: Seq[DefDef] = absValsDefsOps.map {
            case q"..$mods def $name[..$tparams](...$paramss): Eff[$_, $returnType]" =>
              DefDef(mods, name, tparams.dropRight(1) :+ typeU, addImplicits(effectImplicitParams, nonStackParams(paramss)), tq"Eff[U, $returnType]", EmptyTree)
          }
          val runMethod = TermName("run" + sealedTrait.name)
          val cases = absValsDefsOps.map {
            case q"..$mods def $name[..$tparams](...$paramss): Eff[$_, $returnType]" =>
              val binds = nonStackParams(paramss).flatMap(_.collect { case t:ValDef => Bind(t.name, Ident(termNames.WILDCARD))})
              val args = addImplicits(effectImplicitParams, nonStackParams(paramss)).map(_.collect { case t:ValDef => t.name })
              val rhs = q"$name(...$args)"
              cq"${adt(name)}(..$binds) => $rhs.asInstanceOf[Eff[U, X]]"

            case ValDef(_, name, _, _) =>
              cq"${adt(name)} => $name.asInstanceOf[Eff[U, X]]"
          }
          q"""
              trait $factoryTrait[..${(1 to numOutEffects).map(i => TypeDef(Modifiers(Flag.PARAM), TypeName(s"R$i"), List(TypeDef(Modifiers(Flag.PARAM), typeNames.WILDCARD, List(), TypeBoundsTree(TypeTree(), TypeTree()))), TypeBoundsTree(TypeTree(), TypeTree()))
)}] {

                implicit class ${TypeName("Rich" + sealedTrait.name)}[R, A](val effects: Eff[R, A]) {
                  def $runMethod[U](
                    implicit m: Member.Aux[${sealedTrait.name}, R, U],
                    ..$effectImplicitParams
                  ): Eff[U, A] = {
                    $factoryTrait.this.$runMethod(effects)(m, ..$effectImplicitArgs)
                  }
                }

                def $runMethod[R, U, A](effects: Eff[R, A])(
                  implicit m: Member.Aux[${sealedTrait.name}, R, U],
                    ..$effectImplicitParams
                ): Eff[U, A] = {
                  val tr = translator[R, U](m, ..$effectImplicitArgs)
                  org.atnos.eff.interpret.translate(effects)(tr)
                }

                def translator[R, U](
                  implicit m: Member.Aux[${sealedTrait.name}, R, U],
                    ..$effectImplicitParams
                ): org.atnos.eff.Translate[${sealedTrait.name}, U] = new org.atnos.eff.Translate[${sealedTrait.name}, U] {
                    def apply[X](fa: ${sealedTrait.name}[X]): Eff[U, X] = fa match {
                      case ..$cases
                    }
                  }

                ..$methodsToBeImpl
              }

        """
        }

        val functionKTrait =  {
          val methodsToBeImpl: Seq[DefDef] = absValsDefsOps.map {
            case q"..$mods def $name[..$tparams](...$paramss): Eff[$_, $returnType]" =>
              DefDef(mods, name, tparams.dropRight(1), nonStackParams(paramss), tq"M[$returnType]", EmptyTree)
          }

          q"""
              trait FunctionK[M[_]] extends cats.~>[${sealedTrait.name}, M] {
                def apply[X](fa: ${sealedTrait.name}[X]): M[X] = fa match {
                  case ..${absValsDefsOps.map {
                    case DefDef(_, name, _, paramss, rt, _) =>
                      val binds = nonStackParams(paramss).flatMap(_.collect { case t:ValDef => Bind (t.name, Ident(termNames.WILDCARD))})
                      val args = nonStackParams(paramss).map(_.collect { case t:ValDef => Ident(t.name.toTermName) })
                      val rhs = if (args.isEmpty) q"$name" else q"$name(...${args})"
                      cq"${adt(name)}(..$binds) => $rhs.asInstanceOf[M[X]]"
                    case ValDef(_, name, _, _) =>
                      cq"${adt(name)} => $name.asInstanceOf[M[X]]"
                  }}
                }
                ..$methodsToBeImpl
              }
        """
        }
        val genTrait =
          q"""
            trait ${tpname.toTypeName} { $self =>
              import scala.language.higherKinds

              ..${stats.diff(absValsDefsOps)}
              ..$genCaseClassesAndObjADT
              ..$injectOpsObj
              $sideEffectTrait
              $translateTrait
              ..$translatorFactoryTraits
              $functionKTrait
            }
        """

        trees.drop(1).headOption match {
          case Some(q"""$mod object $companionName extends { ..$earlydefns } with ..$parents { ..$body }""")  =>
            val gen = q"""
               $genTrait

               $mod object $companionName extends { ..$earlydefns } with ..$parents with ${tpname.toTypeName} {
                  ..$body
               }
            """
            c.Expr[Any](gen)

          case None =>

            val genCompanionObj =
              q"""
                object ${tpname.toTermName} extends ${tpname.toTypeName}
              """

            val gen = q"""
              $genTrait

              $genCompanionObj
            """

            c.Expr[Any](gen)
        }


      case other => c.abort(c.enclosingPosition, s"${showRaw(other)}")
    }
  }

}
