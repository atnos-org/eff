# Release Process

```
sbt> set every version := "2.0.0"
sbt> project coreJVM
sbt> testOnly *index* -- html.outdir jvm/target/specs2-reports/site html.nostats html console
sbt> testOnly *site* -- html.outdir jvm/target/specs2-reports/site html.nostats html console
sbt> makeSite
sbt> ghpagesPushSite
sbt> project eff
sbt> publishSigned
sbt> sonatypeReleaseAll
```
