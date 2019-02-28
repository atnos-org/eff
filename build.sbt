import ScoverageSbtPlugin._
import org.scalajs.jsenv.nodejs._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val catsVersion        = "1.6.0"
lazy val monixVersion       = "3.0.0-RC2"
lazy val scalazVersion      = "7.2.26"
lazy val specs2Version      = "4.3.6"
lazy val twitterUtilVersion = "18.8.0"
lazy val catbirdVersion     = "18.8.0"
lazy val doobieVersion      = "0.6.0-M2"
lazy val catsEffectVersion  = "1.2.0"

enablePlugins(GhpagesPlugin)
enablePlugins(SitePlugin)
enablePlugins(BuildInfoPlugin)

lazy val eff = project.in(file("."))
  .settings(moduleName := "root")
  .settings(effSettings)
  .settings(noPublishSettings)
  .settings(commonJvmSettings ++ Seq(libraryDependencies ++= scalameter):_*)
  .aggregate(coreJVM, coreJS, doobie, catsEffectJVM, catsEffectJS, macros, monixJVM, monixJS, scalaz, twitter)
  .dependsOn(coreJVM % "test->test;compile->compile", coreJS,
    doobie, catsEffectJVM, catsEffectJS, macros, monixJVM, monixJS, scalaz, twitter)

lazy val core = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(moduleName := "eff")
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jvmSettings(notesSettings:_*)
  .settings(effSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val doobie = project
  .settings(moduleName := "eff-doobie")
  .dependsOn(coreJVM)
  .settings(libraryDependencies ++= doobieJvm)
  .settings(effSettings ++ commonJvmSettings:_*)

lazy val catsEffect = crossProject(JSPlatform, JVMPlatform).in(file("cats"))
  .settings(moduleName := "eff-cats-effect")
  .dependsOn(core)
  .settings(libraryDependencies ++= catsEffectJvm)
  .jsSettings(commonJsSettings ++ Seq(libraryDependencies ++= catsEffectJs.value):_*)
  .jvmSettings(commonJvmSettings:_*)
  .settings(effSettings:_*)

lazy val catsEffectJVM = catsEffect.jvm
lazy val catsEffectJS = catsEffect.js

lazy val macros = project.in(file("macros"))
  .settings(moduleName := "eff-macros")
  .dependsOn(coreJVM)
  .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)
  .settings(addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
  .settings(commonJvmSettings)
  .settings(effSettings:_*)

lazy val monix = crossProject(JSPlatform, JVMPlatform).in(file("monix"))
  .settings(moduleName := "eff-monix")
  .dependsOn(core)
  .settings(libraryDependencies ++= monixLib)
  .settings(effSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings ++ Seq(libraryDependencies ++= monixJs.value):_*)

lazy val monixJVM = monix.jvm
lazy val monixJS =  monix.js

lazy val scalaz = project.in(file("scalaz"))
  .settings(moduleName := "eff-scalaz")
  .dependsOn(coreJVM)
  .settings(libraryDependencies ++= scalazConcurrent)
  .settings(libraryDependencies ++= specs2Scalaz)
  .settings(effSettings ++ commonJvmSettings:_*)

lazy val twitter = project
  .settings(moduleName := "eff-twitter")
  .dependsOn(coreJVM)
  .settings(libraryDependencies ++= twitterUtilCore ++ catbird)
  .settings(effSettings ++ commonJvmSettings:_*)

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageHighlighting := true,
  coverageExcludedPackages := "org\\.atnos\\.eff\\.bench\\..*"
)

lazy val buildSettings = Seq(
  organization := "org.atnos",
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.11.12", scalaVersion.value)
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  resolvers ++= commonResolvers,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
) ++ warnUnusedImport ++ prompt

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  libraryDependencies ++= catsJs.value,
  jsEnv := new NodeJSEnv()
) ++ disableTests

def disableTests = Seq(
  test := {},
  testQuick := {},
  testOnly := {}
)

lazy val commonJvmSettings = Seq(
  fork in Test := true,
  cancelable in Global := true,
  (scalacOptions in Test) ~= (_.filterNot(_ == "-Xfatal-warnings")),
  libraryDependencies ++= catsJvm,
  libraryDependencies ++= specs2
) ++ Seq(scalacOptions in Test ++= Seq("-Yrangepos"))

lazy val effSettings =
  buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings

lazy val publishSettings =
  Seq(
  homepage := Some(url("https://github.com/atnos-org/eff")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/atnos-org/eff"), "scm:git:git@github.com:atnos-org/eff.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://atnos.org/eff/api/")),
  pomExtra := (
    <developers>
      <developer>
        <id>etorreborre</id>
        <name>Eric Torreborre</name>
        <url>https://github.com/etorreborre/</url>
      </developer>
    </developers>
    )
) ++ credentialSettings ++ sharedPublishSettings


lazy val noPublishSettings = Seq(
  publish := (()),
  publishLocal := (()),
  publishArtifact := false
)

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xlint:-nullary-unit",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ypartial-unification",
  "-Xfuture"
)

lazy val sharedPublishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := Option("Releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
) ++ userGuideSettings ++ notesSettings ++ buildInfoSettings

lazy val userGuideSettings =
  Seq(
    ghpagesNoJekyll := false,
    siteSourceDirectory in makeSite := target.value / "specs2-reports" / "site",
    includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js",
    git.remoteRepo := "git@github.com:atnos-org/eff.git"
  )

lazy val notesSettings = Seq(
  ghreleaseRepoOrg := "atnos-org",
  ghreleaseRepoName := "eff",
  ghreleaseTitle := { tagName: TagName => s"eff ${tagName.replace("EFF-", "")}" },
  ghreleaseIsPrerelease := { tagName: TagName => false },
  ghreleaseNotes := { tagName: TagName =>
    // find the corresponding release notes
    val notesFilePath = s"notes/${tagName.replace("EFF-", "")}.markdown"
    try scala.io.Source.fromFile(notesFilePath).mkString
    catch { case t: Throwable => throw new Exception(s"$notesFilePath not found") }
  },
  // just upload the notes
  ghreleaseAssets := Seq()
)

lazy val buildInfoSettings = Seq(
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  buildInfoPackage := "org.atnos.eff"
)

lazy val warnUnusedImport = Seq(
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)

lazy val credentialSettings = Seq(
  // For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)

lazy val prompt = shellPrompt in ThisBuild := { state =>
  val name = Project.extract(state).currentRef.project
  (if (name == "eff") "" else name) + "> "
}

lazy val catsJvm = Seq(
  "org.typelevel" %% "cats-core" % catsVersion)

lazy val catsJs = Def.setting(Seq(
  "org.typelevel" %%% "cats-core" % catsVersion))

lazy val doobieJvm = Seq(
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-h2"   % doobieVersion % "test")

lazy val catsEffectJvm = Seq(
  "org.typelevel" %% "cats-effect" % catsEffectVersion)

lazy val catsEffectJs = Def.setting(Seq(
  "org.typelevel" %%% "cats-effect" % catsEffectVersion))

lazy val monixLib = Seq(
  "io.monix" %% "monix" % monixVersion)

lazy val monixJs = Def.setting(Seq(
  "io.monix" %%% "monix" % monixVersion))

lazy val scalazConcurrent = Seq(
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion)

lazy val specs2Scalaz = Seq(
  "org.specs2" %% "specs2-scalaz" % specs2Version % "test")

lazy val specs2 = Seq(
    "org.specs2" %% "specs2-core"
  , "org.specs2" %% "specs2-matcher-extra"
  , "org.specs2" %% "specs2-scalacheck"
  , "org.specs2" %% "specs2-html"
  , "org.specs2" %% "specs2-junit").map(_ % specs2Version % "test")

lazy val scalameter = Seq(
  "com.storm-enroute" %% "scalameter" % "0.14" % "test")

lazy val twitterUtilCore = Seq(
  "com.twitter" %% "util-collection" % twitterUtilVersion
)

lazy val catbird = Seq(
  "io.catbird" %% "catbird-util" % catbirdVersion
)

lazy val commonResolvers = Seq(
  Resolver.sonatypeRepo("releases")
  , Resolver.typesafeRepo("releases")
  , Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns)
  , Resolver.sonatypeRepo("snapshots")
)
