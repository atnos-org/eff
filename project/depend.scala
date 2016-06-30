import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object depend {

  val catsVersion     = "0.6.0"
  val monixVersion    = "2.0-RC2"
  val specs2Version   = "3.8.4"


  val cats = Seq(
    "org.typelevel" %%%! "cats-core" % catsVersion)

  val monix = Seq(
    "io.monix" %% "monix-eval" % monixVersion)

  val monixjs = Seq(
    "io.monix" %%%! "monix-eval" % monixVersion)

  val specs2 = Seq(
      "org.specs2" %% "specs2-core"
    , "org.specs2" %% "specs2-matcher-extra"
    , "org.specs2" %% "specs2-scalacheck"
    , "org.specs2" %% "specs2-html"
    , "org.specs2" %% "specs2-junit").map(_ % specs2Version % "test")

  val scalameter = Seq(
    "com.storm-enroute" %% "scalameter" % "0.7" % "test")

  val resolvers = Seq(
      Resolver.sonatypeRepo("releases")
    , Resolver.typesafeRepo("releases")
    , Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns))
}
