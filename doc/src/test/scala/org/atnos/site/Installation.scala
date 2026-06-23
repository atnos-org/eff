package org.atnos.site

object Installation extends UserGuidePage {
  def is = "Installation".title ^ s2"""

You add `eff` as an sbt dependency:

```scala
libraryDependencies += "org.atnos" %% "eff" % "$version"

// to write types like Reader[String, *]
// for Scala 3.3.x
scalacOptions += "-Ykind-projector"

// for latest Scala 3
scalacOptions += "-Xkind-projector"
```

##### Additional dependencies

This table lists the other available eff modules:

 Name              | Functionality
 ----------------- | ----------------------------------------------------
 `eff-scalaz`      | if you want to use [Scalaz](https://github.com/scalaz/scalaz) as a library for functional programming. This gives you a `Scalaz` `Monad` instance for `Eff` and a Scalaz's `\/` effect
 `eff-monix`       | to use Monix's `Task` effect
 `eff-cats-effect` | to use cats's `IO` effect
 `eff-doobie`      | to use Doobie's `ConnectionIO` effect

<p/>

### Imports

#### Main types

The main `eff` types: `Eff`, `Member`, `Fx` are accessible in the `org.atnos.eff` package:
```scala
import org.atnos.eff._
```

Many other effects are also available $OutOfTheBox.

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
import org.atnos.eff.syntax.all.given
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
