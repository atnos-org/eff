# Release Process

```
sbt> set every version := "2.0.0"
sbt> cd coreJVM
sbt> testOnly *index* -- html.outdir jvm/target/site html.nostats html console
sbt> testOnly *site* -- html.outdir jvm/target/site html.nostats html console
sbt> ghpagesPushSite
sbt> cd eff
sbt> publishSigned
sbt> sonatypeReleaseAll
```
