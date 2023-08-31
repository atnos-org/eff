addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")
addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")
addSbtPlugin("com.github.sbt" % "sbt-git" % "2.0.1")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.21")
addSbtPlugin("ohnosequences" % "sbt-github-release" % "0.7.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.14")

// https://github.com/ohnosequences/sbt-github-release/issues/28#issuecomment-426086656
libraryDependencies += "com.sun.activation" % "javax.activation" % "1.2.0"
