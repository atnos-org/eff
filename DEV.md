# Release Process

```
export GITHUB_OAUTH=<token>
sbt> set every version := "5.0.0"
sbt> testOnly *index* -- html html.search html.toc html.nostats html.outdir jvm/target/specs2-reports/site all
sbt> cd coreJVM
sbt> ghpagesPushSite
sbt> githubRelease EFF-<tag name>
sbt> cd eff
sbt> +publishSigned
sbt> sonatypeReleaseAll
```

# Running the benchmarks

```
test:run
```