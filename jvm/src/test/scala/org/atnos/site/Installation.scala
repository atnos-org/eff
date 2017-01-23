package org.atnos.site

object Installation extends UserGuidePage { def is = "Installation".title ^ s2"""

You add `eff` as an sbt dependency:

```scala
libraryDependencies += "org.atnos" %% "eff" % "2.0.0"

// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

// to get types like Reader[String, ?] (with more than one type parameter) correctly inferred
addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")
```

If you want to use [Scalaz](http://github.com/scalaz/scalaz) as a library for functional programming you will also need:
```scala
libraryDependencies += "org.atnos" %% "eff-scalaz" % "2.0.0"
```

This will allows you to use the `Eff` monad as a Scalaz `Monad` (instead of the cats `Monad` which is the default implementation).
The `eff-scalaz` dependency is also necessary if you want to use scalaz `Task` as an effect.
(see $OutOfTheBox).

On the other hand if you prefer to use [Monix](http://monix.io) `Task`, you will need
a dependency on `eff-monix`:

```scala
libraryDependencies += "org.atnos" %% "eff-monix" % "2.0.0"
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

You can also import all effects at once:
```scala
import org.atnos.eff.all._
```

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
