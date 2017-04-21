package org.atnos.site

object Installation extends UserGuidePage { def is = "Installation".title ^ s2"""

You add `eff` as an sbt dependency:

```scala
libraryDependencies += "org.atnos" %% "eff" % "$version"

// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
```

To get types like Reader[String, ?] (with more than one type parameter) correctly inferred, you'll to use the following compiler option

```scala
scalacOptions += "-Ypartial-unification"
```

If you want to use [Scalaz](http://github.com/scalaz/scalaz) as a library for functional programming you will also need:
```scala
libraryDependencies += "org.atnos" %% "eff-scalaz" % "$version"
```

This will allows you to use the `Eff` monad as a Scalaz `Monad` (instead of the cats `Monad` which is the default implementation).
The `eff-scalaz` dependency is also necessary if you want to use scalaz `Task` as an effect.
(see $OutOfTheBox).

On the other hand if you prefer to use [Monix](http://monix.io) `Task`, you will need
a dependency on `eff-monix`:

```scala
libraryDependencies += "org.atnos" %% "eff-monix" % "$version"
```

### Imports

#### Main types

The main `eff` types: `Eff`, `Member`, `Fx` are accessible in the `org.atnos.eff` package:
```scala
import org.atnos.eff._
```

#### Creating effects

The functions used to create effects are grouped under different objects named after the effect type. For example if you want to create the `Eval` effect you need to
import:
```scala
import org.atnos.eff.eval._
```

You can also import most of the effects at once with:
```scala
import org.atnos.eff.all._
```

The only effects not included in the previous import are:

 - the `Error` effect. This effect requires a type parameter representing the "failure" type and must be provided by the user of the library

 - the `Future` effect. This effect shares some operations like `runAsync` with other "async" effects like monix's `TaskEffect`
    and the import could clash with `import org.atnos.eff.addon.monix.task._`


#### Interpreting effects

Interpreting effects usually requires some syntax to "run" a given effect. For example to "run" the `Option` effect you will import:
```scala
// to create the effect
import org.atnos.eff.option._

// to access the runOption method
import org.atnos.eff.syntax.option._

fromOption(Option(1)).runOption
```

You can also access all the syntax imports at once with:
```scala
import org.atnos.eff.syntax.all._
```

#### Intellij support

Intellij error highlighting doesn't support implicit-directed type inference yet, check https://youtrack.jetbrains.com/issue/SCL-11140 or https://youtrack.jetbrains.com/issue/SCL-10753 for progress.

#### With Scalaz

If you use Scalaz as your functional programming library you might need additional imports in order to use some creation
methods specific to Scalaz. For example:

```scala
import org.atnos.eff.addon.scalaz.either._

fromDisjunction(\/-(1))
```

There is also an `all` object importing all those methods at once:
```scala
import org.atnos.eff.addon.scalaz.all._

fromDisjunction(\/-(1))
```

And you can already guess, there are some syntax imports following the same pattern:
```scala
import org.atnos.eff.addon.scalaz.either._
import org.atnos.eff.addon.scalaz.syntax.either._

fromDisjunction(\/-(1)).runDisjunction
```

"""
}
