# Release Process

```
# edit version.sbt and README.md
# create and edit notes/"new version".markdown
# git tag "new version"
# git push git@github.com:atnos-org/eff.git "new version"

sbt> +publishSigned
sbt> sonaRelease

# set SNAPSHOT version.sbt
```

# Running the benchmarks

```
test:run
```
