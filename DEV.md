# Release Process

```
# create and edit notes/"new version".markdown
# git tag "new version"
# git push git@github.com:atnos-org/eff.git "new version"

export GITHUB_OAUTH=<token>
sbt> set every version := "new version"
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
