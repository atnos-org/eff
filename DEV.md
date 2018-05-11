# Release Process

```
export GITHUB_OAUTH=<token>
sbt> set every version := "5.3.0"
sbt> testOnly *index* -- html html.search html.toc html.nostats html.outdir target/specs2-reports/site all
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