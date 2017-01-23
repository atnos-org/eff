package org.atnos.eff
package syntax

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
trait async extends
  AsyncInterpretation

@deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
object async extends async
