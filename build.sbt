import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import ReleaseTransformations._
import ScoverageSbtPlugin._
import com.ambiata.promulgate.project.ProjectPlugin.promulgate
import org.scalajs.sbtplugin.cross.CrossType
import Defaults.{defaultTestTasks, testTaskOptions}
import sbtrelease._

lazy val eff = project.in(file("."))
  .settings(moduleName := "root")
  .settings(effSettings)
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS, macros, monixJVM, monixJS, scalaz, twitter, fs2JS, fs2JVM)
  .dependsOn(coreJVM, coreJS, macros, monixJVM, monixJS, scalaz, twitter, fs2JS, fs2JVM)

lazy val core = crossProject.crossType(CrossType.Full).in(file("."))
  .settings(moduleName := "eff")
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings ++ Seq(libraryDependencies ++= scalameter):_*)
  .jvmSettings(promulgate.library("org.atnos.eff", "eff"):_*)
  .jvmSettings(notesSettings:_*)
  .settings(effSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val macros = project.in(file("macros"))
  .settings(moduleName := "eff-macros")
  .dependsOn(coreJVM)
  .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)
  .settings(addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch))
  .settings(commonJvmSettings)
  .settings(effSettings:_*)

lazy val monix = crossProject.crossType(CrossType.Full).in(file("monix"))
  .settings(moduleName := "eff-monix")
  .dependsOn(core)
  .settings(libraryDependencies ++= monixEval)
  .settings(effSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings ++ Seq(libraryDependencies ++= monixJs):_*)

lazy val monixJVM = monix.jvm
lazy val monixJS =  monix.js

lazy val scalaz = project.in(file("scalaz"))
  .settings(moduleName := "eff-scalaz")
  .dependsOn(coreJVM)
  .settings(libraryDependencies ++= scalazConcurrent)
  .settings(libraryDependencies ++= specs2Scalaz)
  .settings(effSettings ++ commonJvmSettings:_*)

lazy val fs2 = crossProject.crossType(CrossType.Full).in(file("fs2"))
  .settings(moduleName := "eff-fs2")
  .dependsOn(core)
  .settings(effSettings:_*)
  .jvmSettings(commonJvmSettings ++ Seq(libraryDependencies ++= fs2Jvm):_*)
  .jsSettings(commonJsSettings ++ Seq(libraryDependencies ++= fs2Js):_*)

lazy val fs2JVM = fs2.jvm
lazy val fs2JS =  fs2.js

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
  scalaVersion := "2.12.2",
  crossScalaVersions := Seq("2.11.11", scalaVersion.value)
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  resolvers ++= commonResolvers,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
) ++ warnUnusedImport ++ prompt

lazy val tagName = Def.setting{
  s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  requiresDOM := false,
  libraryDependencies ++= catsJs,
  jsEnv := NodeJSEnv().value
) ++ disableTests

def disableTests = Seq(
  test := {},
  testQuick := {},
  testOnly := {}
)

lazy val commonJvmSettings = Seq(
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
) ++ credentialSettings ++ sharedPublishSettings ++ sharedReleaseProcess


lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
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
  releaseCrossBuild := true,
  releaseTagName := tagName.value,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := Option("Releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
) ++ site.settings ++
  ghpages.settings ++
  userGuideSettings

lazy val userGuideSettings =
  Seq(
    GhPagesKeys.ghpagesNoJekyll := false,
    SiteKeys.siteSourceDirectory in SiteKeys.makeSite := target.value / "specs2-reports" / "site",
    includeFilter in SiteKeys.makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js",
    git.remoteRepo := "git@github.com:atnos-org/eff.git"
  )

lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies
  , inquireVersions
  , runTest
  , setReleaseVersion
  , commitReleaseVersion
  , tagRelease
  , generateWebsite
  , publishSite
  , publishArtifacts
  , setNextVersion
  , commitNextVersion
  , ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true)
  , pushChanges
  )
) ++
  Seq(
    releaseNextVersion := { v => Version(v).map(_.bumpBugfix.string).getOrElse(versionFormatError) },
    releaseTagName := "EFF-" + releaseVersion.value(version.value)
  ) ++
  testTaskDefinition(generateWebsiteTask, Seq(Tests.Filter(_.endsWith("Website"))))

lazy val publishSite = ReleaseStep { st: State =>
  val st2 = executeStepTask(makeSite, "Making the site")(st)
  executeStepTask(pushSite, "Publishing the site")(st2)
}

lazy val notesSettings = Seq(
  ghreleaseRepoOrg := "atnos-org",
  ghreleaseRepoName := "eff",
  ghreleaseTitle := { tagName: TagName => s"eff ${tagName.replace("EFF-", "")}" },
  ghreleaseIsPrerelease := { tagName: TagName => false },
  ghreleaseNotes := { tagName: TagName =>
    // find the corresponding release notes
    val notesFilePath = s"notes/${tagName.replace("EFF-", "")}.markdown"
    try io.Source.fromFile(notesFilePath).mkString
    catch { case t: Throwable => throw new Exception(s"$notesFilePath not found") }
  },
  // just upload the notes
  ghreleaseAssets := Seq()
)

lazy val generateWebsiteTask = TaskKey[Tests.Output]("generate-website", "generate the website")
lazy val generateWebsite     = executeStepTask(generateWebsiteTask, "Generating the website", Test)

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

def executeTask(task: TaskKey[_], info: String) = (st: State) => {
  st.log.info(info)
  val extracted = Project.extract(st)
  val ref: ProjectRef = extracted.get(thisProjectRef)
  extracted.runTask(task in ref, st)._1
}

def executeStepTask(task: TaskKey[_], info: String, configuration: Configuration) = ReleaseStep { st: State =>
  executeTask(task, info, configuration)(st)
}

def executeStepTask(task: TaskKey[_], info: String) = ReleaseStep { st: State =>
  executeTask(task, info)(st)
}

def executeTask(task: TaskKey[_], info: String, configuration: Configuration) = (st: State) => {
  st.log.info(info)
  Project.extract(st).runTask(task in configuration, st)._1
}

def testTaskDefinition(task: TaskKey[Tests.Output], options: Seq[TestOption]) =
  Seq(testTask(task))                          ++
    inScope(GlobalScope)(defaultTestTasks(task)) ++
    inConfig(Test)(testTaskOptions(task))        ++
    (testOptions in (Test, task) ++= options)

def testTask(task: TaskKey[Tests.Output]) =
  task := Def.taskDyn {
    Def.task(
      Defaults.allTestGroupsTask(
        (streams in Test).value,
        (loadedTestFrameworks in Test).value,
        (testLoader in Test).value,
        (testGrouping in Test in test).value,
        (testExecution in Test in task).value,
        (fullClasspath in Test in test).value,
        (javaHome in test).value
      )).flatMap(identity)
  }.value

lazy val catsVersion   = "0.9.0"
lazy val monixVersion  = "2.3.0"
lazy val scalazVersion = "7.2.7"
lazy val fs2Version    = "0.9.2"
lazy val specs2Version = "3.8.9"
lazy val twitterUtilVersion = "6.42.0"
lazy val catbirdVersion = "0.13.0"

lazy val catsJvm = Seq(
  "org.typelevel" %% "cats-core" % catsVersion)

lazy val catsJs = Seq(
  "org.typelevel" %%%! "cats-core" % catsVersion)

lazy val monixEval = Seq(
  "io.monix" %% "monix-eval" % monixVersion,
  "io.monix" %% "monix-cats" % monixVersion)

lazy val monixJs = Seq(
  "io.monix" %%%! "monix-eval" % monixVersion,
  "io.monix" %%%! "monix-cats" % monixVersion)

lazy val scalazConcurrent = Seq(
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion)

lazy val specs2Scalaz = Seq(
  "org.specs2" %% "specs2-scalaz" % specs2Version % "test")

lazy val fs2Jvm = Seq(
  "co.fs2" %% "fs2-core" % fs2Version)

lazy val fs2Js = Seq(
  "co.fs2" %%%! "fs2-core" % fs2Version)

lazy val specs2 = Seq(
    "org.specs2" %% "specs2-core"
  , "org.specs2" %% "specs2-matcher-extra"
  , "org.specs2" %% "specs2-scalacheck"
  , "org.specs2" %% "specs2-html"
  , "org.specs2" %% "specs2-junit").map(_ % specs2Version % "test")

lazy val scalameter = Seq(
  "com.storm-enroute" %% "scalameter" % "0.8.2" % "test")

lazy val twitterUtilCore = Seq(
  "com.twitter" %% "util-collection" % twitterUtilVersion
)

lazy val catbird = Seq(
  "io.catbird" %% "catbird-util" % catbirdVersion
)

lazy val commonResolvers = Seq(
  Resolver.sonatypeRepo("releases")
  , Resolver.typesafeRepo("releases")
  , Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns))
