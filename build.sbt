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
  .settings(promulgate.library("org.atnos.eff", "eff-cats"):_*)
  .aggregate(coreJVM, coreJS, monixJVM, monixJS)
  .dependsOn(coreJVM, coreJS, monixJVM, monixJS)

lazy val core = crossProject.crossType(CrossType.Full).in(file("."))
  .settings(moduleName := "eff-cats")
  .jsSettings(commonJsSettings:_*)
  .jvmSettings(commonJvmSettings ++ Seq(libraryDependencies ++= depend.scalameter):_*)
  .settings(effSettings:_*)

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val monix = crossProject.crossType(CrossType.Full).in(file("monix"))
  .settings(moduleName := "eff-cats-monix")
  .dependsOn(core)
  .settings(libraryDependencies ++= depend.monix)
  .settings(effSettings:_*)
  .jvmSettings(commonJvmSettings:_*)
  .jsSettings(commonJsSettings ++ Seq(libraryDependencies ++= depend.monixjs):_*)

lazy val monixJVM = monix.jvm
lazy val monixJS =  monix.js

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageHighlighting := true,
  coverageExcludedPackages := "org\\.atnos\\.eff\\.bench\\..*"
)

lazy val buildSettings = Seq(
  organization := "org.atnos",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.11.8")
)

lazy val commonSettings = Seq(
  scalacOptions ++= commonScalacOptions,
  resolvers ++= depend.resolvers,
  libraryDependencies ++= depend.cats,
  scalacOptions in (Compile, doc) := (scalacOptions in (Compile, doc)).value.filter(_ != "-Xfatal-warnings"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),
  addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")
) ++ warnUnusedImport ++ prompt

lazy val tagName = Def.setting{
  s"v${if (releaseUseGlobalVersion.value) (version in ThisBuild).value else version.value}"
}

lazy val commonJsSettings = Seq(
  scalaJSStage in Global := FastOptStage,
  parallelExecution := false,
  requiresDOM := false,
  jsEnv := NodeJSEnv().value
)

lazy val commonJvmSettings = Seq(
  libraryDependencies ++= depend.specs2
)

lazy val effSettings =
  buildSettings ++ commonSettings ++ publishSettings ++ scoverageSettings

lazy val publishSettings =
  Seq(
  homepage := Some(url("https://github.com/atnos-org/eff-cats")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/atnos-org/eff-cats"), "scm:git:git@github.com:atnos-org/eff-cats.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://atnos.org/eff-cats/api/")),
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
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
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
    git.remoteRepo := "git@github.com:atnos-org/eff-cats.git"
  )

lazy val sharedReleaseProcess = Seq(
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies
  , inquireVersions
//  , runTest
  , setReleaseVersion
  , commitReleaseVersion
  , tagRelease
//  , generateWebsite
//  , publishSite
//  , publishArtifacts
//  , setNextVersion
//  , commitNextVersion
//  , ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true)
//  , pushChanges
  )
) ++
  Seq(
    releaseNextVersion := { v => Version(v).map(_.bumpBugfix.string).getOrElse(versionFormatError) },
    releaseTagName <<= (releaseVersion, version) map  { (rv, v) => "EFF-" + rv(v) }
  ) ++
  testTaskDefinition(generateWebsiteTask, Seq(Tests.Filter(_.endsWith("Website"))))

lazy val publishSite = ReleaseStep { st: State =>
  val st2 = executeStepTask(makeSite, "Making the site")(st)
  executeStepTask(pushSite, "Publishing the site")(st2)
}

lazy val generateWebsiteTask = TaskKey[Tests.Output]("generate-website", "generate the website")
lazy val generateWebsite     = executeStepTask(generateWebsiteTask, "Generating the website", Test)

lazy val warnUnusedImport = Seq(
  scalacOptions in (Compile, console) ~= {_.filterNot("-Ywarn-unused-import" == _)},
  scalacOptions in (Test, console) <<= (scalacOptions in (Compile, console))
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
  task <<= (streams in Test, loadedTestFrameworks in Test, testLoader in Test,
    testGrouping in Test in test, testExecution in Test in task,
    fullClasspath in Test in test, javaHome in test) flatMap Defaults.allTestGroupsTask
