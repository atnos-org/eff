import sbt._
import Keys._

object depend {

  val catsVersion     = "0.5.0"
  val specs2Version   = "3.7.3"

  val cats = Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.typelevel" %% "cats-laws" % catsVersion % "test" excludeAll(
        ExclusionRule("org.typelevel", "discipline"),
        ExclusionRule("org.scalacheck", "scalacheck"))) ++
  Seq("org.typelevel" %% "discipline" % "0.5" % "test")


  val specs2 = Seq(
      "org.specs2" %% "specs2-core"
    , "org.specs2" %% "specs2-matcher-extra"
    , "org.specs2" %% "specs2-scalacheck"
    , "org.specs2" %% "specs2-html"
    , "org.specs2" %% "specs2-junit").map(_ % specs2Version % "test")

  val scalameter = Seq(
    "com.storm-enroute" %% "scalameter" % "0.7")

  val resolvers = Seq(
      Resolver.sonatypeRepo("releases")
    , Resolver.typesafeRepo("releases")
    , Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns))
}
