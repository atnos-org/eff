import sbt._
import Keys._
import com.ambiata.promulgate.project.ProjectPlugin.promulgate
import xerial.sbt.Sonatype._

object build extends Build {
  type Settings = Def.Setting[_]

  lazy val project = Project(
    id = "eff",
    base = file("."),
    settings = Defaults.coreDefaultSettings ++
               dependencies             ++
               projectSettings          ++
               compilationSettings      ++
               testingSettings          ++
               publicationSettings
    ).configs(Benchmark).settings(inConfig(Benchmark)(Defaults.testSettings):_*)

  lazy val dependencies: Seq[Settings] =
    Seq[Settings](libraryDependencies ++=
      depend.cats       ++
      depend.specs2     ++
      depend.disorder   ++
      depend.scalameter
    ) ++
    Seq(resolvers := depend.resolvers)

  lazy val projectSettings: Seq[Settings] = Seq(
    name := "eff-cats",
    version in ThisBuild := "1.0.0",
    organization := "org.specs2",
    scalaVersion := "2.11.7")

  lazy val compilationSettings: Seq[Settings] = Seq(
    javacOptions ++= Seq("-Xmx3G", "-Xms512m", "-Xss4m"),
    maxErrors := 20,
    scalacOptions ++= Seq("-Xfatal-warnings",
            "-Xlint",
            "-Ywarn-unused-import",
            "-Yno-adapted-args",
            "-Ywarn-numeric-widen",
            "-Ywarn-value-discard",
            "-deprecation:false", "-Xcheckinit", "-unchecked", "-feature", "-language:_"),
    scalacOptions in Test ++= Seq("-Yrangepos"),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
  )

  lazy val testingSettings: Seq[Settings] = Seq(
    initialCommands in (Test, console) := "import org.specs2._",
    testFrameworks := Seq(TestFrameworks.Specs2, new TestFramework("org.scalameter.ScalaMeterFramework")),
    logBuffered := false,
    cancelable := true,
    javaOptions += "-Xmx3G",
    parallelExecution in Benchmark := false
  )

  lazy val Benchmark = config("bench") extend Test

  lazy val publicationSettings: Seq[Settings] =
    promulgate.library("org.specs2.info.eff", "specs2") ++
    Seq(
    publishTo in Global <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      Some("staging" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>http://specs2.org/</url>
        <licenses>
          <license>
            <name>MIT-style</name>
            <url>http://www.opensource.org/licenses/mit-license.php</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>http://github.com/etorreborre/eff</url>
          <connection>scm:http:http://etorreborre@github.com/etorreborre/eff.git</connection>
        </scm>
        <developers>
          <developer>
            <id>etorreborre</id>
            <name>Eric Torreborre</name>
            <url>http://etorreborre.blogspot.com/</url>
          </developer>
        </developers>
    ),
    credentials := Seq(Credentials(Path.userHome / ".sbt" / "specs2.credentials"))
  ) ++
  sonatypeSettings
}
