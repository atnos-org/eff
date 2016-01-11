package org.specs2.site

import org.specs2.Specification
import org.specs2.main.Arguments
import org.specs2.specification.Snippets
import org.specs2.specification.core._

abstract class UserGuidePage extends Specification with Snippets {

  /** don't display statistics */
  override def map(structure: SpecStructure): SpecStructure =
    Arguments("html.nostats") ^ structure

  /** mute all links, so that they are not decorated in the html */
  override def map(fs: =>Fragments): Fragments =
    fs.map {
      case Fragment(r: SpecificationRef, e, l) => Fragment(r.mute, e, l)
      case f => f
    }

}

