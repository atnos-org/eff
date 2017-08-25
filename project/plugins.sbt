addSbtPlugin("com.github.gseitz"    % "sbt-release"           % "1.0.0")
addSbtPlugin("com.jsuereth"         % "sbt-pgp"               % "1.0.0")
addSbtPlugin("com.typesafe.sbt"     % "sbt-ghpages"           % "0.5.4")
addSbtPlugin("com.typesafe.sbt"     % "sbt-site"              % "0.8.2")
addSbtPlugin("org.scoverage"        % "sbt-scoverage"         % "1.5.0")
addSbtPlugin("com.typesafe.sbt"     % "sbt-git"               % "0.8.5")
addSbtPlugin("org.scala-js"         % "sbt-scalajs"           % "0.6.19")
addSbtPlugin("org.xerial.sbt"       % "sbt-sonatype"          % "1.1")
addSbtPlugin("com.ambiata"          % "promulgate"            % "0.11.0-20150727222014-93879fa")
addSbtPlugin("ohnosequences"        % "sbt-github-release"    % "0.4.0")

resolvers += Resolver.url("sonatype", new URL("https://oss.sonatype.org/content/repositories/releases"))(Resolver.ivyStylePatterns)
resolvers += Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns)
resolvers += "Era7 maven releases" at "https://s3-eu-west-1.amazonaws.com/releases.era7.com"
resolvers += "Jenkins repo" at "http://repo.jenkins-ci.org/public/"
