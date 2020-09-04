# Release Process

```
# create and edit notes/"new version".markdown

export GITHUB_OAUTH=<token>
sbt> set every version := "new version"
sbt> testOnly *index* -- html html.search html.toc html.nostats html.outdir target/specs2-reports/site all
sbt> ghpagesPushSite
sbt> set every ghreleaseGithubToken := Some("GITHUB TOKEN")
sbt> githubRelease <tag name>
sbt> cd eff
sbt> +publishSigned
sbt> sonatypeBundleRelease
```

# Running the benchmarks

```
test:run
```
