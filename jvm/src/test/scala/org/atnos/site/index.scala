import org.atnos.site._

object index extends UserGuidePage { def is = "eff".title ^ s2"""

Extensible effects are an alternative to monad transformers for computing with effects in a functional way.
This library is based on the "free-er" monad and an "open union" of effects described by
Oleg Kiselyov in [Freer monads, more extensible effects](http://okmij.org/ftp/Haskell/extensible/more.pdf).

You can learn more in the following sections:

 - ${"installation and imports" ~ Installation}
 - ${"your first effects" ~ Introduction}
 - ${"standard effects in eff" ~ OutOfTheBox}: `Reader`, `Writer`, `Eval`, `State`,...
 - ${"create your own effects" ~/ CreateEffects}
 - ${"interpret and manipulate effect stacks" ~ TransformStack}
 - ${"use Member implicits" ~/ MemberImplicits }
 - ${"use an applicative evaluation" ~ ApplicativeEvaluation}

### Contributing

`eff` is a [Typelevel](http://typelevel.org) project. This means we embrace pure, typeful, functional programming,
and provide a safe and friendly environment for teaching, learning, and contributing as described in the [Typelevel Code of Conduct](http://typelevel.org/conduct.html).

Feel free to open an issue if you notice a bug, have an idea for a feature, or have a question about the code. Pull requests are also gladly accepted.

"""

}
