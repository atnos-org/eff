import org.scalajs.jsenv.nodejs._

Global / concurrentRestrictions += Tags.limit(NativeTags.Link, 1)

lazy val specs2Version = "4.23.0"
lazy val doobieVersion = "0.13.4"
val Scala3 = "3.3.8"
val scalaVersions = Seq(Scala3)
val defaultAxis = VirtualAxis.scalaABIVersion(Scala3)

def hash() = sys.process.Process("git rev-parse HEAD").lazyLines_!.head

val effRoot = rootProject.autoAggregate.settings(
  moduleName := "root",
  scalaVersion := Scala3,
  effSettings,
  noPublishSettings,
)

val effDoc = project
  .in(file("doc"))
  .settings(
    moduleName := "eff-doc",
    scalaVersion := Scala3,
    effSettings,
    noPublishSettings,
    commonJvmSettings,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "org.atnos.eff",
    libraryDependencies += "org.specs2" %% "specs2-html" % specs2Version % "test"
  )
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(
    all.jvm(Scala3) % "test->test;compile->compile",
    doobie.jvm(Scala3),
    monix.jvm(Scala3),
    scalaz.jvm(Scala3),
  )

def p(id: String) = ProjectMatrix(id = id, base = file(id), pluginClassLoader = this.getClass.getClassLoader)
  .defaultAxes(defaultAxis)
  .settings(moduleName := s"eff-$id")
  .jsPlatform(scalaVersions, commonJsSettings)
  .jvmPlatform(scalaVersions, commonJvmSettings)
  .settings(
    effSettings,
  )
  .nativePlatform(scalaVersions, commonNativeSettings)
  .dependsOn(core % "compile->compile;test->test")

lazy val core = projectMatrix
  .defaultAxes(defaultAxis)
  .settings(moduleName := "eff-core")
  .jsPlatform(scalaVersions, commonJsSettings)
  .jvmPlatform(scalaVersions, commonJvmSettings)
  .settings(
    effSettings,
  )
  .nativePlatform(scalaVersions, commonNativeSettings)

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

lazy val all = projectMatrix
  .defaultAxes(defaultAxis)
  .in(file("all"))
  .settings(moduleName := "eff")
  .jsPlatform(scalaVersions, commonJsSettings)
  .jvmPlatform(scalaVersions, commonJvmSettings)
  .settings(
    effSettings,
  )
  .nativePlatform(
    scalaVersions,
    Def.settings(
      commonNativeSettings,
      Test / testOptions ++= {
        if (scala.util.Properties.isLinux) {
          Seq(Tests.Exclude(Set("org.atnos.eff.EvalEffectSpec")))
        } else {
          Nil
        }
      },
    )
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

lazy val doobie = projectMatrix
  .settings(moduleName := "eff-doobie")
  .dependsOn(core % "compile->compile;test->test")
  .jvmPlatform(scalaVersions)
  .settings(libraryDependencies ++= doobieJvm)
  .settings(effSettings ++ commonJvmSettings)

lazy val catsEffect = projectMatrix
  .defaultAxes(defaultAxis)
  .in(file("cats"))
  .settings(moduleName := "eff-cats-effect")
  .dependsOn(future, option % Test)
  .settings(
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.7.0",
  )
  .jvmPlatform(scalaVersions, commonJvmSettings)
  .nativePlatform(scalaVersions, commonNativeSettings)
  .settings(effSettings)

lazy val monix = projectMatrix
  .defaultAxes(defaultAxis)
  .in(file("monix"))
  .settings(moduleName := "eff-monix")
  .dependsOn(
    core % "compile->compile;test->test",
    option % Test,
  )
  .settings(
    libraryDependencies += "io.monix" %% "monix-eval" % "3.4.1",
  )
  .settings(effSettings)
  .jvmPlatform(scalaVersions, commonJvmSettings)
  .jsPlatform(scalaVersions, commonJsSettings)

lazy val scalaz = projectMatrix
  .defaultAxes(defaultAxis)
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
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.9",
  )
  .settings(effSettings)
  .jvmPlatform(scalaVersions, commonJvmSettings)
  .jsPlatform(scalaVersions, commonJsSettings)
  .nativePlatform(scalaVersions, commonNativeSettings)

lazy val buildSettings = Seq(
  organization := "org.atnos",
)

lazy val commonSettings = Def.settings(
  semanticdbEnabled := true,
  libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0",
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
        _.withESFeatures(_.withUseWebAssembly(true).withESVersion(org.scalajs.linker.interface.ESVersion.ES2022)).withModuleKind(ModuleKind.ESModule)
      ),
      jsEnv := Def.uncached {
        import org.scalajs.jsenv.nodejs.NodeJSEnv
        val config = NodeJSEnv
          .Config()
          .withArgs(
            List(
              "--experimental-wasm-exnref",
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
  evictionErrorLevel := Level.Warn,
  libraryDependencies ++= specs2.value,
  Test / testFull := {
    if ((Test / sources).value.isEmpty) {
      TestResult.Empty
    } else {
      (Test / testFull).value
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
    "-language:strictEquality",
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:existentials",
    "-unchecked",
    "-no-indent",
    "-Ykind-projector",
  )
}

lazy val sharedPublishSettings = Seq(
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := Function.const(false),
  publishTo := (if (isSnapshot.value) None else localStaging.value),
)

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
    "org.specs2" %% "specs2-core",
    "org.specs2" %% "specs2-matcher-extra",
    "org.specs2" %% "specs2-scalacheck",
    "org.specs2" %% "specs2-junit",
  ).map(
    _ % specs2Version % "test"
  )
)

inThisBuild(
  Seq(
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixDependencies += "com.github.xuwei-k" %% "scalafix-rules" % "0.6.28"
  )
)
