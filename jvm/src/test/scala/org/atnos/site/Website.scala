package org.atnos.site

import org.specs2._
import org.specs2.execute._
import runner._

class Website extends Specification { def is = s2"""

  Generate the website
    the user guide $userGuide

"""
  val pages = Seq(
    "ApplicativeEvaluation"
  , "CreateEffects"
  , "MemberImplicits"
  , "Introduction"
  , "OutOfTheBox"
  , "Tutorial"
  , "TransformStack"
  ).map("org.atnos.site."+_) :+ "index"

  def userGuide = {
    Result.foreach(pages) { page =>
      ClassRunner.run(s"$page html html.outdir jvm/target/specs2-reports/site html.nostats html.verbose".split(" "))
      ok
    }
  }

}
