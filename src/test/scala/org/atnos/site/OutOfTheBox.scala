package org.atnos.site

import lib._

object OutOfTheBox extends UserGuidePage {
  def is = "Out of the box".title ^ s2"""

This library comes with the following effects:

 Name                | Description                                                                      | Link
 ---------           | -------------------------------------------                                      | -----
 `EvalEffect`        | an effect for delayed computations                                               | ${"link" ~ EvalEffectPage}
 `OptionEffect`      | an effect for optional computations, stopping when there's no available value    | ${"link" ~ OptionEffectPage}
 `EitherEffect`      | an effect for computations with failures, stopping when there is a failure       | ${"link" ~ EitherEffectPage}
 `ValidateEffect`    | an effect for computations with failures, allowing to collect failures           | ${"link" ~ ValidateEffectPage}
 `ErrorEffect`       | a mix of Eval and Either, catching exceptions and returning them as failures     | ${"link" ~ ErrorEffectPage}
 `ReaderEffect`      | an effect for depending on a configuration or an environment                     | ${"link" ~ ReaderEffectPage}
 `WriterEffect`      | an effect to log messages                                                        | ${"link" ~ WriterEffectPage}
 `StateEffect`       | an effect to pass state around                                                   | ${"link" ~ StateEffectPage}
 `ListEffect`        | an effect for computations returning several values                              | ${"link" ~ ListEffectPage}
 `ChooseEffect`      | an effect for modeling non-determinism                                           | ${"link" ~ ChooseEffectPage}
 `MemoEffect`        | an effect for memoizing values                                                   | ${"link" ~ MemoEffectPage}
 `FutureEffect`      | an effect for asynchronous computations                                          | ${"link" ~ TimedFutureEffectPage}
 `TaskEffect`        | an effect for asynchronous computations using Monix Tasks                        | ${"link" ~ TaskEffectPage}
 `SafeEffect`        | an effect for guaranteeing resource safety                                       | ${"link" ~ SafeEffectPage}

<small>(from `org.atnos.eff._`)</small>

Other modules listed in $Installation provide additional effects (Twitter Future, Scalaz Task, Doobie ConnectionIO,...).

<br/>

## What's next?

Now you can learn how to  ${"create your own effects" ~/ CreateEffects}!
"""

}
