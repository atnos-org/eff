import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.SbtGhPages.GhPagesKeys._
import ReleaseTransformations._
import ScoverageSbtPlugin._
import com.ambiata.promulgate.project.ProjectPlugin.promulgate
import org.scalajs.sbtplugin.cross.CrossType
import Defaults.{defaultTestTasks, testTaskOptions}
import sbtrelease._
import org.scalajs.jsenv.nodejs._

lazy val catsVersion        = "1.0.0-MF"
lazy val monixVersion       = "3.0.0-M1"
lazy val scalazVersion      = "7.2.7"
lazy val specs2Version      = "4.0.0-RC5"
lazy val twitterUtilVersion = "7.1.0"
lazy val catbirdVersion     = "0.18.0"
lazy val doobieVersion      = "0.5.0-M8"
lazy val catsEffectVersion  = "0.4"

lazy val eff = project.in(file("."))
  .settings(moduleName := "root")
  .settings(effSettings)
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS, doobie, cats, macros, monixJVM, monixJS, scalaz, twitter)
  .dependsOn(coreJVM, coreJS, doobie, cats, macros, monixJVM, monixJS, scalaz, twitter)

lazy val core = crossProject.crossType(CrossType.Full).in(file("."))
  .settings(moduleName := "eff")
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings ++ Seq(libraryDependencies ++= scalameter):_*)
  .jvmSettings(promulgate.library("org.atnos.eff", "eff"):_*)
  .jvmSettings(notesSettings:_*)
  .settings(effSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val doobie = project
  .settings(moduleName := "eff-doobie")
  .dependsOn(coreJVM)
  .settings(libraryDependencies ++= doobieJvm)
  .settings(effSettings ++ commonJvmSettings:_*)

lazy val cats = project.in(file("cats"))
  .settings(moduleName := "eff-cats-effect")
  .dependsOn(coreJVM)
  .settings(libraryDependencies ++= catsEffectJvm)
  .settings(effSettings ++ commonJvmSettings:_*)

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
  .settings(libraryDependencies ++= monixLib)
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
  scalaVersion := "2.12.3",
  crossScalaVersions := Seq("2.11.11", scalaVersion.value)
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  resolvers ++= commonResolvers,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
) ++ warnUnusedImport ++ prompt

lazy val tagName = Def.setting{
  s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  requiresDOM := false,
  libraryDependencies ++= catsJs,
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

lazy val catsJvm = Seq(
  "org.typelevel" %% "cats-core" % catsVersion)

lazy val catsJs = Seq(
  "org.typelevel" %%%! "cats-core" % catsVersion)

lazy val doobieJvm = Seq(
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-h2"   % doobieVersion % "test")

lazy val catsEffectJvm = Seq(
  "org.typelevel" %% "cats-effect" % catsEffectVersion)

lazy val monixLib = Seq(
  "io.monix" %% "monix" % monixVersion)

lazy val monixJs = Seq(
  "io.monix" %%%! "monix" % monixVersion)

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
  , Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns)
  , Resolver.sonatypeRepo("snapshots")
)
