package org.atnos.site

import org.specs2.control.ioOperationToOption
import org.specs2.execute.Snippet
import org.specs2.io.*
import org.specs2.matcher.*
import org.specs2.specification.Snippets
import org.specs2.specification.core.*
import org.specs2.specification.create.SpecificationCreation
import scala.reflect.ClassTag

abstract class UserGuidePage
    extends ImmutableSpecificationStructure
    with SpecificationCreation
    with Snippets
    with TypedEqual
    with ExpectationsCreation {

  /** mute all links, so that they are not decorated in the html */
  override def map(fs: => Fragments): Fragments =
    fs.map {
      case Fragment(r: SpecificationRef, e, l) => Fragment(r.mute, e, l)
      case f => f
    }

  def load(path: FilePath): Snippet[Unit] =
    Snippet(
      code = () => (),
      codeExpression = FileSystem.readFile(path).runOption.orElse(Option("no file found at " + path.path))
    )

  def definition[T: ClassTag]: Snippet[Unit] = {
    val name = summon[ClassTag[T]].runtimeClass.getName
    load(FilePath.unsafe("src/test/scala/" + name.replace(".", "/") + ".scala"))
  }

  val version: String = {
    val v = org.atnos.eff.BuildInfo.version
    val suffix = "-SNAPSHOT"
    if (v.endsWith(suffix)) {
      v.dropRight(suffix.length).split('.').toSeq match {
        case xs :+ x =>
          x.toIntOption.filter(_ > 0).fold(v) { a =>
            (xs :+ (a - 1).toString).mkString(".")
          }
        case _ =>
          v
      }
    } else {
      v
    }
  }

  override given defaultSnippetParameters[T]: org.specs2.execute.SnippetParams[T] =
    Snippet.defaultParams[T].copy(asCode = (s1: String, s2: String) => splitText(Snippet.markdownCode()(s1, s2)))

  // extract //text comments as text paragraphs
  def splitText(code: String): String =
    code
      .split("\n")
      .foldLeft((Vector[String](), true)) { case ((res, isCode), line) =>
        if (line.trim startsWith "/*p")
          (res :+ "\n```\n" + line.trim.drop(3), false)
        else if (line.endsWith("*/") && !isCode)
          (res :+ "```", true)
        else
          (res :+ line, isCode)
      }
      ._1
      .mkString("\n")

  extension [T](snippet: Snippet[T]) {
    def noPrompt: Snippet[T] =
      snippet.copy(params = defaultSnippetParameters[T].copy(prompt = Snippet.emptyPrompt))
  }
}
