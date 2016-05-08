import sbt._
import Keys._
import com.ambiata.promulgate.project.ProjectPlugin.promulgate
import xerial.sbt.Sonatype._
import com.typesafe.sbt.SbtSite.{SiteKeys, site}
import com.typesafe.sbt.SbtGhPages.{GhPagesKeys, ghpages}
import com.typesafe.sbt.SbtGit.git
import org.scalajs.sbtplugin.cross.{CrossProject, CrossType}

object build extends Build {
  type Settings = Def.Setting[_]

  lazy val root = project.in(file(".")).
    aggregate(js, jvm).
    settings(
      publish := {},
      publishLocal := {}
    )

  lazy val effProject = CrossProject("eff", new File("."), CrossType.Full).
    settings((
      Defaults.coreDefaultSettings ++
      sharedDependencies           ++
      projectSettings              ++
      compilationSettings          ++
      publicationSettings
      ):_*).
    jvmSettings((
      jvmDependencies ++
      testingSettings
    ):_*).configs(Benchmark).settings(inConfig(Benchmark)(Defaults.testSettings):_*)

  lazy val jvm = effProject.jvm
  lazy val js = effProject.js

  lazy val sharedDependencies: Seq[Settings] =
    Seq[Settings](libraryDependencies ++=
      depend.cats
    ) ++
    Seq(resolvers := depend.resolvers)

  lazy val jvmDependencies: Seq[Settings] =
    Seq[Settings](libraryDependencies ++=
      depend.specs2     ++
      depend.scalameter
    ) ++
    Seq(resolvers := depend.resolvers)

  lazy val projectSettings: Seq[Settings] = Seq(
    name := "eff-cats",
    version in ThisBuild := "1.6.1",
    organization := "org.atnos",
    scalaVersion := "2.11.8")

  lazy val compilationSettings: Seq[Settings] = Seq(
    scalaBinaryVersion := "2.11",
    resolvers += "scalatl" at "http://milessabin.com/scalatl",
    javacOptions ++= Seq("-Xmx3G", "-Xms512m", "-Xss4m"),
    maxErrors := 20,
    triggeredMessage := Watched.clearWhenTriggered,
    scalacOptions ++= Seq(
            "-Xfatal-warnings",
            "-Xlint",
            "-Yno-adapted-args",
            "-Ywarn-numeric-widen",
            "-Ywarn-value-discard",
            "-Ywarn-unused-import",
            "-Ywarn-dead-code",
            "-deprecation:false", "-Xcheckinit", "-unchecked", "-feature", "-language:_"),
    scalacOptions in Test ++= Seq("-Yrangepos"), //, "-Xlog-implicits"),
    scalacOptions in Test ~= (_.filterNot(Set("-Ywarn-dead-code"))),
    scalacOptions in console := Seq(),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),
    addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")
  )

  lazy val testingSettings: Seq[Settings] = Seq(
    initialCommands in (Test, console) := "import org.specs2._",
    testFrameworks := Seq(TestFrameworks.Specs2, new TestFramework("org.scalameter.ScalaMeterFramework")),
    logBuffered := false,
    cancelable := true,
    javaOptions += "-Xmx3G",
    parallelExecution := false
  )
  lazy val Benchmark = config("bench") extend Test

  lazy val publicationSettings: Seq[Settings] =
    promulgate.library("org.atnos.eff", "atnos") ++
    Seq(
    publishTo in Global <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      Some("staging" at nexus + "service/local/staging/deploy/maven2")
    },
    autoAPIMappings := true,
    apiURL := Some(url("https://etorreborre.github.io/eff-cats/api/")),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>http://github.com/etorreborre/eff-cats/</url>
        <licenses>
          <license>
            <name>MIT-style</name>
            <url>http://www.opensource.org/licenses/mit-license.php</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>http://github.com/atnos-org/eff-cats</url>
          <connection>scm:git:git@github.com:atnos-org/eff-cats.git</connection>
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
  sonatypeSettings ++
  site.settings ++
  ghpages.settings ++
  Seq(
    GhPagesKeys.ghpagesNoJekyll := false,
    SiteKeys.siteSourceDirectory := target.value / "specs2-reports" / "site",
    includeFilter in SiteKeys.makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js",
    git.remoteRepo := "git@github.com:etorreborre/eff-cats.git"
  )

}
