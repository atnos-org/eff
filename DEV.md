# Release Process

```
sbt> set every version := "4.0.0"
sbt> cd coreJVM
sbt> testOnly *index* -- html.outdir jvm/target/specs2-reports/site html.nostats html console
sbt> testOnly *site* -- html.outdir jvm/target/specs2-reports/site html.nostats html console
sbt> ghpagesPushSite
sbt> githubRelease EFF-<tag name>
sbt> cd eff
sbt> +publishSigned
sbt> sonatypeReleaseAll
```
