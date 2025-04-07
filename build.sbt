import org.scalajs.jsenv.nodejs._
import sbtcrossproject.CrossProject

Global / concurrentRestrictions += Tags.limit(NativeTags.Link, 1)

lazy val specs2Version = "4.21.0"
lazy val doobieVersion = "0.13.4"

enablePlugins(BuildInfoPlugin)

def hash() = sys.process.Process("git rev-parse HEAD").lineStream_!.head

moduleName := "root"
effSettings
noPublishSettings
commonJvmSettings
libraryDependencies += "org.specs2" %% "specs2-html" % specs2Version % "test"

dependsOn(
  all.jvm % "test->test;compile->compile",
  doobie,
  monixJVM,
  scalazJVM,
)

def p(id: String) = CrossProject(id, file(id))(JSPlatform, JVMPlatform, NativePlatform)
  .settings(moduleName := s"eff-$id")
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .settings(
    effSettings,
  )
  .nativeSettings(commonNativeSettings)
  .dependsOn(core % "compile->compile;test->test")

lazy val core = CrossProject("core", file("core"))(JSPlatform, JVMPlatform, NativePlatform)
  .settings(moduleName := "eff-core")
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .settings(
    effSettings,
  )
  .nativeSettings(commonNativeSettings)

lazy val eval = p("eval")
lazy val option = p("option")
lazy val either = p("either")
lazy val validate = p("validate")
lazy val error = p("error")
lazy val reader = p("reader")
lazy val writer = p("writer")
lazy val choose = p("choose")
lazy val list = p("list")
lazy val state = p("state")
lazy val safe = p("safe").dependsOn(either)
lazy val future = p("future").dependsOn(eval)

lazy val all = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(moduleName := "eff")
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .jvmSettings(notesSettings)
  .settings(
    effSettings,
  )
  .nativeSettings(
    commonNativeSettings,
    Test / testOptions ++= {
      if (scala.util.Properties.isLinux) {
        Seq(Tests.Exclude(Set("org.atnos.eff.EvalEffectSpec")))
      } else {
        Nil
      }
    },
  )
  .dependsOn(
    core % "test->test",
    eval,
    option,
    either,
    validate,
    error,
    reader,
    writer,
    choose,
    list,
    state,
    safe,
    future
  )

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val doobie = project
  .settings(moduleName := "eff-doobie")
  .dependsOn(coreJVM % "compile->compile;test->test")
  .settings(libraryDependencies ++= doobieJvm)
  .settings(effSettings ++ commonJvmSettings)

lazy val catsEffect = crossProject(JVMPlatform)
  .in(file("cats"))
  .settings(moduleName := "eff-cats-effect")
  .dependsOn(future, option % Test)
  .settings(
    libraryDependencies += "org.typelevel" %%% "cats-effect" % "3.6.1",
  )
  .jvmSettings(commonJvmSettings)
  .settings(effSettings)

lazy val catsEffectJVM = catsEffect.jvm

lazy val monix = crossProject(JSPlatform, JVMPlatform)
  .in(file("monix"))
  .settings(moduleName := "eff-monix")
  .dependsOn(
    core % "compile->compile;test->test",
    option % Test,
  )
  .settings(
    libraryDependencies += "io.monix" %%% "monix-eval" % "3.4.1",
  )
  .settings(effSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)

lazy val monixJVM = monix.jvm
lazy val monixJS = monix.js

lazy val scalaz = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalaz"))
  .settings(moduleName := "eff-scalaz")
  .dependsOn(
    core % "test->test",
    option % Test,
    choose % Test,
    either,
    validate,
    eval,
    safe,
  )
  .settings(
    libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.3.8",
  )
  .settings(effSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)

lazy val scalazJVM = scalaz.jvm
lazy val scalazJS = scalaz.js

lazy val buildSettings = Seq(
  organization := "org.atnos",
  scalaVersion := "3.3.5"
)

lazy val commonSettings = Def.settings(
  semanticdbEnabled := scalaVersion.value.startsWith("3.3"),
  libraryDependencies += "org.typelevel" %%% "cats-core" % "2.13.0",
  scalacOptions ++= commonScalacOptions.value,
  (Compile / doc / scalacOptions) ++= {
    Seq(
      "-sourcepath",
      (LocalRootProject / baseDirectory).value.getAbsolutePath,
      "-doc-source-url",
      s"https://github.com/atnos-org/eff/tree/${hash()}€{FILE_PATH}.scala"
    )
  },
  (Compile / doc / scalacOptions) := (Compile / doc / scalacOptions).value.filter(_ != "-Xfatal-warnings"),
) ++ warnUnusedImport ++ prompt

lazy val commonJsSettings = Def.settings(
  libraryDependencies ++= specs2.value,
  parallelExecution := false,
  if (sys.props.isDefinedAt("scala_js_wasm")) {
    println("enable wasm")
    Def.settings(
      scalaJSLinkerConfig ~= (
        _.withExperimentalUseWebAssembly(true).withModuleKind(ModuleKind.ESModule)
      ),
      jsEnv := {
        import org.scalajs.jsenv.nodejs.NodeJSEnv
        val config = NodeJSEnv
          .Config()
          .withArgs(
            List(
              "--experimental-wasm-exnref",
              "--experimental-wasm-imported-strings",
              "--turboshaft-wasm",
            )
          )
        new NodeJSEnv(config)
      },
    )
  } else {
    Def.settings()
  },
  scalacOptions += {
    val a = (LocalRootProject / baseDirectory).value.toURI.toString
    val g = "https://raw.githubusercontent.com/atnos-org/eff/" + hash()
    val key = "-scalajs-mapSourceURI"
    s"${key}:$a->$g/"
  }
)

lazy val commonJvmSettings = Seq(
  libraryDependencies ++= specs2.value,
  Test / fork := true,
  Global / cancelable := true,
  Test / scalacOptions ~= (_.filterNot(_ == "-Xfatal-warnings")),
)

lazy val commonNativeSettings = Def.settings(
  libraryDependencies ++= specs2.value,
  Test / test := {
    if ((Test / sources).value.isEmpty) {
      ()
    } else {
      (Test / test).value
    }
  },
)

lazy val effSettings =
  buildSettings ++ commonSettings ++ publishSettings

lazy val publishSettings =
  Seq(
    homepage := Some(url("https://github.com/atnos-org/eff")),
    licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT")),
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
  ) ++ sharedPublishSettings

lazy val noPublishSettings = Seq(
  publish := (()),
  publishLocal := (()),
  publishArtifact := false
)

lazy val commonScalacOptions = Def.setting {
  Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:implicitConversions,higherKinds,existentials",
    "-unchecked",
    "-no-indent",
    "-Ykind-projector",
  )
}

lazy val sharedPublishSettings = Seq(
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := Function.const(false),
  publishTo := sonatypePublishToBundle.value,
) ++ notesSettings

lazy val notesSettings = Seq(
  ghreleaseRepoOrg := "atnos-org",
  ghreleaseRepoName := "eff",
  ghreleaseTitle := { (tagName: TagName) => s"eff ${tagName.replace("EFF-", "")}" },
  ghreleaseIsPrerelease := { (tagName: TagName) => false },
  ghreleaseNotes := { (tagName: TagName) =>
    // find the corresponding release notes
    val notesFilePath = s"notes/${tagName.replace("EFF-", "")}.markdown"
    try scala.io.Source.fromFile(notesFilePath).mkString
    catch { case t: Throwable => throw new Exception(s"$notesFilePath not found", t) }
  },
  // just upload the notes
  ghreleaseAssets := Seq()
)

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "org.atnos.eff"

lazy val warnUnusedImport = Seq(
  scalacOptions += "-Wunused:imports",
  Compile / console / scalacOptions ~= { _.filterNot("-Wunused:imports" == _) },
  (Test / console / scalacOptions) := (Compile / console / scalacOptions).value
)

lazy val prompt = ThisBuild / shellPrompt := { state =>
  val name = Project.extract(state).currentRef.project
  (if (name == "eff") "" else name) + "> "
}

lazy val doobieJvm = Seq("org.tpolecat" %% "doobie-core" % doobieVersion, "org.tpolecat" %% "doobie-h2" % doobieVersion % "test")

lazy val specs2 = Def.setting(
  Seq(
    "org.specs2" %%% "specs2-core",
    "org.specs2" %%% "specs2-matcher-extra",
    "org.specs2" %%% "specs2-scalacheck",
    "org.specs2" %%% "specs2-junit",
  ).map(
    _ % specs2Version % "test"
  )
)

Global / excludeLintKeys ++= Set(
  ghreleaseIsPrerelease,
  ghreleaseNotes,
  ghreleaseTitle,
)

inThisBuild(
  Seq(
    semanticdbVersion := scalafixSemanticdb.revision
  )
)
