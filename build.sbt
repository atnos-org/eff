import org.scalajs.jsenv.nodejs._

lazy val specs2Version = Def.setting("4.15.0")
lazy val twitterUtilVersion = "22.4.0"
lazy val doobieVersion = "0.13.4"

enablePlugins(BuildInfoPlugin)

def hash() = sys.process.Process("git rev-parse HEAD").lineStream_!.head

moduleName := "root"
effSettings
noPublishSettings
commonJvmSettings
libraryDependencies ++= scalameter.value
libraryDependencies += "org.specs2" %% "specs2-html" % specs2Version.value % "test"
disableScala3

dependsOn(
  coreJVM % "test->test;compile->compile",
  doobie,
  macros,
  monixJVM,
  scalazJVM,
)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(moduleName := "eff")
  .jsSettings(commonJsSettings)
  .jvmSettings(commonJvmSettings)
  .jvmSettings(notesSettings)
  .settings(
    effSettings,
  )
  .nativeSettings(commonNativeSettings)

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
  .dependsOn(core)
  .settings(
    libraryDependencies += "org.typelevel" %%% "cats-effect" % "3.3.11",
  )
  .jvmSettings(commonJvmSettings)
  .settings(effSettings)

lazy val catsEffectJVM = catsEffect.jvm

lazy val macros = project
  .in(file("macros"))
  .settings(moduleName := "eff-macros")
  .dependsOn(coreJVM)
  .settings(libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value)
  .settings(
    disableScala3,
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some(2, v) if v <= 12 =>
          Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch))
        case _ =>
          // if scala 2.13.0-M4 or later, macro annotations merged into scala-reflect
          // https://github.com/scala/scala/pull/6606
          Nil
      }
    }
  )
  .settings(commonJvmSettings)
  .settings(effSettings)

lazy val monix = crossProject(JSPlatform, JVMPlatform)
  .in(file("monix"))
  .settings(moduleName := "eff-monix")
  .dependsOn(core % "test->test;compile->compile")
  .settings(
    libraryDependencies += "io.monix" %%% "monix" % "3.4.1",
  )
  .settings(effSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)

lazy val monixJVM = monix.jvm
lazy val monixJS = monix.js

lazy val scalaz = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalaz"))
  .settings(moduleName := "eff-scalaz")
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    libraryDependencies += "org.scalaz" %%% "scalaz-core" % "7.3.6",
  )
  .settings(effSettings)
  .jvmSettings(commonJvmSettings)
  .jsSettings(commonJsSettings)
  .nativeSettings(commonNativeSettings)

lazy val scalazJVM = scalaz.jvm
lazy val scalazJS = scalaz.js

lazy val twitter = project
  .settings(moduleName := "eff-twitter")
  .dependsOn(coreJVM % "compile->compile;test->test")
  .settings(libraryDependencies ++= twitterUtilCore)
  .settings(effSettings ++ commonJvmSettings)
  .settings(
    conflictWarning := {
      if (scalaBinaryVersion.value == "3") {
        // TODO
        ConflictWarning("warn", Level.Warn, false)
      } else {
        conflictWarning.value
      }
    },
  )

def Scala212 = "2.12.15"

lazy val buildSettings = Seq(
  organization := "org.atnos",
  scalaVersion := Scala212,
  crossScalaVersions := Seq(Scala212, "2.13.8", "3.1.2")
)

lazy val commonSettings = Def.settings(
  libraryDependencies += "org.typelevel" %%% "cats-core" % "2.7.0",
  scalacOptions ++= commonScalacOptions.value,
  Compile / doc / scalacOptions ++= {
    Seq(
      "-sourcepath",
      (LocalRootProject / baseDirectory).value.getAbsolutePath,
      "-doc-source-url",
      s"https://github.com/atnos-org/eff/tree/${hash()}€{FILE_PATH}.scala"
    )
  },
  Compile / doc / scalacOptions := (Compile / doc / scalacOptions).value.filter(_ != "-Xfatal-warnings"),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some(3, _) =>
        Nil
      case _ =>
        Seq(
          compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)
        )
    }
  }
) ++ warnUnusedImport ++ prompt

lazy val commonJsSettings = Seq(
  parallelExecution := false,
  libraryDependencies ++= specs2.value,
  scalacOptions ++= {
    if (scalaBinaryVersion.value == "3") {
      Nil
    } else {
      Seq("-P:scalajs:nowarnGlobalExecutionContext")
    }
  },
  scalacOptions += {
    val a = (LocalRootProject / baseDirectory).value.toURI.toString
    val g = "https://raw.githubusercontent.com/atnos-org/eff/" + hash()
    val key = CrossVersion.partialVersion(scalaVersion.value) match {
      case Some(3, _) =>
        "-scalajs-mapSourceURI"
      case _ =>
        "-P:scalajs:mapSourceURI"
    }
    s"${key}:$a->$g/"
  }
)

lazy val commonJvmSettings = Seq(
  libraryDependencies ++= specs2.value,
  Test / fork := true,
  Global / cancelable := true,
  Test / scalacOptions ~= (_.filterNot(_ == "-Xfatal-warnings")),
  Test / scalacOptions ++= {
    if (scalaBinaryVersion.value == "3") {
      Nil
    } else {
      Seq("-Yrangepos")
    }
  },
)

lazy val commonNativeSettings = Def.settings(
  disableScala3, // TODO https://github.com/typelevel/cats/issues/4117
  Test / loadedTestFrameworks := Map.empty,
  Test / sources := Nil,
  test := {},
  Test / testOnly := {},
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
    pomExtra :=
      <developers>
      <developer>
        <id>etorreborre</id>
        <name>Eric Torreborre</name>
        <url>https://github.com/etorreborre/</url>
      </developer>
    </developers>
  ) ++ credentialSettings ++ sharedPublishSettings

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
  ) ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some(2, _) =>
        Seq(
          "-Xlint",
          "-Xlint:-nullary-unit",
          "-Ywarn-numeric-widen",
          "-Ywarn-value-discard",
        )
      case _ =>
        Nil
    }
  } ++ {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some(2, v) if v >= 13 =>
        Seq(
          "-Ymacro-annotations"
        )
      case Some(3, _) =>
        Seq(
          "-no-indent",
          "-Ykind-projector",
          "-source",
          "3.0-migration",
        )
      case _ =>
        Seq(
          "-Xfatal-warnings",
          "-Yno-adapted-args",
          "-Ypartial-unification",
          "-Xfuture"
        )
    }
  }
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
  Compile / console / scalacOptions ~= { _.filterNot("-Ywarn-unused-import" == _) },
  Test / console / scalacOptions := (Compile / console / scalacOptions).value
)

lazy val credentialSettings = Seq(
  // For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
  credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
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
    _ % specs2Version.value % "test"
  ).map(
    _.exclude(
      "org.specs2",
      "xml_sjs1_" + scalaBinaryVersion.value
    )
  )
)

lazy val scalameter = Def.setting(
  if (scalaBinaryVersion.value == "3") {
    Nil
  } else {
    Seq("com.storm-enroute" %% "scalameter" % "0.19" % "test")
  }
)

lazy val twitterUtilCore = Seq(
  "com.twitter" %% "util-core" % twitterUtilVersion
)

lazy val disableScala3 = Def.settings(
  libraryDependencies := {
    if (scalaBinaryVersion.value == "3") {
      Nil
    } else {
      libraryDependencies.value
    }
  },
  Seq(Compile, Test).map { x =>
    x / sources := {
      if (scalaBinaryVersion.value == "3") {
        Nil
      } else {
        (x / sources).value
      }
    }
  },
  Test / test := {
    if (scalaBinaryVersion.value == "3") {
      ()
    } else {
      (Test / test).value
    }
  },
  publish / skip := scalaBinaryVersion.value == "3",
)

Global / excludeLintKeys ++= Set(
  ghreleaseIsPrerelease,
  ghreleaseNotes,
  ghreleaseTitle,
)
