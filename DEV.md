# Release Process

```
sbt> set every version := "4.0.0"
sbt> cd coreJVM
sbt> testOnly *index* -- html html.search html.toc html.nostats html.outdir jvm/target/specs2-reports/site all
sbt> ghpagesPushSite
sbt> githubRelease EFF-<tag name>
sbt> cd eff
sbt> +publishSigned
sbt> sonatypeReleaseAll
```
