# Development notes

How to release the user guide:
```
sbt> project effJVM
sbt> testOnly *index* -- html.nostats html.outdir target/site html
sbt> testOnly *site* -- html.nostats html.outdir target/site html
sbt> project root
sbt> makeSite
sbt> ghpagesPushSite
```
