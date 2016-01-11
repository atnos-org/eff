scalacOptions += "-deprecation"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += Resolver.typesafeRepo("releases")

resolvers += Resolver.url("ambiata-oss", new URL("https://ambiata-oss.s3.amazonaws.com"))(Resolver.ivyStylePatterns)

resolvers += "Era7 maven releases" at "http://releases.era7.com.s3.amazonaws.com"

addSbtPlugin("com.ambiata" % "promulgate" % "0.11.0-20150727222014-93879fa")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.4.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "0.8.2")

resolvers += "jgit-repo" at "http://download.eclipse.org/jgit/maven"

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.5.4")
