package org.atnos.site

import org.specs2.matcher._
import org.specs2.execute.Snippet
import org.specs2.io._
import org.specs2.specification.Snippets
import org.specs2.specification.core._
import org.specs2.specification.create.SpecificationCreation

import scala.reflect.ClassTag

abstract class UserGuidePage extends
  ImmutableSpecificationStructure with
  SpecificationCreation with
  Snippets with
  TypedEqual with
  ExpectationsCreation {

  /** mute all links, so that they are not decorated in the html */
  override def map(fs: =>Fragments): Fragments =
    fs.map {
      case Fragment(r: SpecificationRef, e, l) => Fragment(r.mute, e, l)
      case f => f
    }

  def load(path: FilePath): Snippet[Unit] =
    Snippet(
      code = () => (),
      codeExpression = FileSystem.readFile(path).runOption.orElse(Option("no file found at "+path.path))
    )

  def definition[T : ClassTag]: Snippet[Unit] = {
    val name = implicitly[ClassTag[T]].runtimeClass.getName
    load(FilePath.unsafe("src/test/scala/"+name.replace(".", "/")+".scala"))
  }
}

