import org.atnos.site.*

object index extends UserGuidePage {
  def is = "eff".title ^ s2"""

Extensible effects are an alternative to monad transformers for computing with effects in a functional way.
This library is based on the "free-er" monad and an "open union" of effects described by
Oleg Kiselyov in [Freer monads, more extensible effects](https://okmij.org/ftp/Haskell/extensible/more.pdf).

You can learn more in the following sections, it is recommended to read them in order if you are new to `eff`:

 1. ${"installation and imports" ~ Installation}
 1. ${"your first effects" ~ Introduction}
 1. ${"standard effects in eff" ~ OutOfTheBox}: `Reader`, `Writer`, `Eval`, `State`,...
 1. ${"tutorial" ~ Tutorial}
 1. ${"create your own effects" ~ CreateEffects}
 1. ${"interpret and manipulate effect stacks" ~ TransformStack}
 1. ${"use Member implicits" ~ MemberImplicits}
 1. ${"use an applicative evaluation" ~ ApplicativeEvaluation}
 1. ${"tips and tricks" ~ Cookbook}
 1. ${"community resources" ~ CommunityResources}

### Contributing

`eff` is a [Typelevel](https://typelevel.org) project. This means we embrace pure, typeful, functional programming,
and provide a safe and friendly environment for teaching, learning, and contributing as described in the [Scala Code of Conduct](https://www.scala-lang.org/conduct/).

Feel free to open an issue if you notice a bug, have an idea for a feature, or have a question about the code. Pull requests are also gladly accepted.

"""

}
