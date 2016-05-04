# eff

[![Build Status](https://travis-ci.org/atnos-org/eff-cats.png?branch=master)](https://travis-ci.org/atnos-org/eff-cats)
[![Join the chat at https://gitter.im/atnos-org/eff-cats](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/atnos-org/eff-cats?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Extensible effects are an alternative to monad transformers for computing with effects in a functional way.
This library is based on the "free-er" monad and an "open union" of effects described in
Oleg Kiselyov in [Freer monads, more extensible effects](http://okmij.org/ftp/Haskell/extensible/more.pdf).

You can learn more in the User Guide:

 - [your first effects](http://atnos-org.github.io/eff-cats/org.atnos.site.Introduction.html)
 - [included effects: `Reader`, `Writer`, `Eval`, `State`,...](http://atnos-org.github.io/eff-cats/org.atnos.site.OutOfTheBox.html)
 - [how to use implicits to get type-inference right](http://atnos-org.github.io/eff-cats/org.atnos.site.Implicits.html)
 - [using an open or a closed union of effects](http://atnos-org.github.io/eff-cats/org.atnos.site.OpenClosed.html)
 - [create your own effects](http://atnos-org.github.io/eff-cats/org.atnos.site.CreateEffects.html)
 - [working with different effect stacks](http://atnos-org.github.io/eff-cats/org.atnos.site.TransformStack.html)

## Installation

You add `eff-cats` as an sbt dependency:
```scala
libraryDependencies += "org.atnos" %% "eff-cats" % "1.5"

// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),

// to get types like Reader[String, ?] (with more than one type parameter) correctly inferred
addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.1.0")
```

or download it from [here](https://oss.sonatype.org/content/repositories/releases/org/atnos/eff-cats_2.11/1.5/eff-cats_2.11-1.5.jar).

# Contributing

[eff-cats](https://github.com/atnos-org/eff-cats/) is a [Typelevel](http://typelevel.org) project. This means we embrace pure, typeful, functional programming,
and provide a safe and friendly environment for teaching, learning, and contributing as described in the [Typelevel Code of Conduct](http://typelevel.org/conduct.html).

Feel free to open an issue if you notice a bug, have an idea for a feature, or have a question about the code. Pull requests are also gladly accepted.
