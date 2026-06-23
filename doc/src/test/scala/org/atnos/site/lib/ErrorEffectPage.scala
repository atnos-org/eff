package org.atnos.site
package lib

object ErrorEffectPage extends UserGuidePage {
  def is = "Error".title ^ s2"""

The `Error` effect is both an `Eval` effect and a `Either` one with `Throwable Either F` on the "left" side.
  The idea is to represent computations which can fail, either with an exception or a failure. You can:

  - create delayed computations with `ok`

- fail with `fail(f: F)` where `F` is the failure type

- throw an exception with `exception`

Other useful combinators are available:

  - `andFinally(last)` registers an action to be executed even in case of an exception or a failure

- `orElse` runs an action and then run another one if the first is not successful

- `whenFailed` does the same thing than `orElse` but uses the error for `action1` to decide which action to run next

When you run an `Error` effect you get back an `Error Either A` where `Error` is a type alias for `Throwable Either Failure`.

  The `Error` object implements this effect with `String` as the `Failure` type but you are encouraged to create our own
  failure datatype and extend the `Error[MyFailureDatatype]` trait.

"""
}
