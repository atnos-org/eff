# eff

[![Build Status](https://travis-ci.org/atnos-org/eff.png?branch=master)](https://travis-ci.org/atnos-org/eff)
[![Join the chat at https://gitter.im/atnos-org/eff](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/atnos-org/eff?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Extensible effects are an alternative to monad transformers for computing with effects in a functional way.
This library is based on the "free-er" monad and extensible effects described in
Oleg Kiselyov in [Freer monads, more extensible effects](http://okmij.org/ftp/Haskell/extensible/more.pdf).

You can learn more in the User Guide:

 - [your first effects](http://atnos-org.github.io/eff/org.atnos.site.Introduction.html)
 - [included effects: `Reader`, `Writer`, `Eval`, `State`,...](http://atnos-org.github.io/eff/org.atnos.site.OutOfTheBox.html)
 - [create your own effects](http://atnos-org.github.io/eff/org.atnos.site.CreateEffects.html)
 - [use Member implicits](http://atnos-org.github.io/eff/org.atnos.site.MemberImplicits.html)
 - [working with different effect stacks](http://atnos-org.github.io/eff/org.atnos.site.TransformStack.html)
 - [a tutorial similar to the cats' tutorial for Free monads](http://atnos-org.github.io/eff/org.atnos.site.Tutorial.html)

You can also check out [this presentation](http://bit.ly/eff_flatmap_2016) at flatMap Oslo 2016 ([slides](http://www.slideshare.net/etorreborre/the-eff-monad-one-monad-to-rule-them-all)).

## Installation

You add `eff` as an sbt dependency:
```scala
libraryDependencies += "org.atnos" %% "eff" % "4.0.0"

// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

// to get types like Reader[String, ?] (with more than one type parameter) correctly inferred for scala 2.11 < 2.11.9
// you can use the [Typelevel Scala compiler](http://typelevel.org/scala)
scalaOrganization in ThisBuild := "org.typelevel"

// to get types like Reader[String, ?] (with more than one type parameter) correctly inferred for scala 2.12.x
scalacOptions += "-Ypartial-unification"

```

# Contributing

[eff](https://github.com/atnos-org/eff/) is a [Typelevel](http://typelevel.org) project. This means we embrace pure, typeful, functional programming,
and provide a safe and friendly environment for teaching, learning, and contributing as described in the [Typelevel Code of Conduct](http://typelevel.org/conduct.html).

Feel free to open an issue if you notice a bug, have an idea for a feature, or have a question about the code. Pull requests are also gladly accepted.
