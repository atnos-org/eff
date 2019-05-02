addSbtPlugin("com.jsuereth"         % "sbt-pgp"               % "1.1.2")
addSbtPlugin("com.typesafe.sbt"     % "sbt-ghpages"           % "0.6.3")
addSbtPlugin("com.typesafe.sbt"     % "sbt-site"              % "1.3.2")
addSbtPlugin("org.scoverage"        % "sbt-scoverage"         % "1.5.1")
addSbtPlugin("com.typesafe.sbt"     % "sbt-git"               % "1.0.0")
addSbtPlugin("org.scala-js"         % "sbt-scalajs"           % "0.6.27")
addSbtPlugin("org.portable-scala"   % "sbt-scalajs-crossproject" % "0.6.0")
addSbtPlugin("org.xerial.sbt"       % "sbt-sonatype"          % "2.3")
addSbtPlugin("ohnosequences"        % "sbt-github-release"    % "0.7.0")
addSbtPlugin("com.eed3si9n"         % "sbt-buildinfo"         % "0.9.0")

// https://github.com/ohnosequences/sbt-github-release/issues/28#issuecomment-426086656
libraryDependencies += "com.sun.activation" % "javax.activation" % "1.2.0"

resolvers += Resolver.url("sonatype", new URL("https://oss.sonatype.org/content/repositories/releases"))(Resolver.ivyStylePatterns)
