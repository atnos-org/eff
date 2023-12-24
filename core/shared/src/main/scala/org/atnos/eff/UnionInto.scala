package org.atnos.eff

/**
 * transform a Union for a given stack into a Union for another stack
 */
trait UnionInto[R, S] {

  def apply[A](union: Union[R, A]): Union[S, A]

  /** transform the unions for a given effect */
  def into[A](e: Eff[R, A]): Eff[S, A] =
    e match {
      case Pure(a, last) =>
        Eff.pure(a).addLast(last.interpret(into))

      case Impure(NoEffect(a), c, l) =>
        Impure(NoEffect(a), c.interpretEff(into)(into), l.interpret(into))

      case Impure(u: Union[?, ?], c, l) =>
        Impure(apply(u), c.interpretEff(into)(into), l.interpret(into))

      case ImpureAp(unions, continuation, last) =>
        ImpureAp(unions.into(this), continuation.interpretEff(into)(into), last.interpret(into))
    }
}
