package org.atnos.eff

/**
 * Effect for delayed computations
 *
 * uses cats.Eval as a supporting data structure
 */
trait EvalEffect extends EvalTypes with EvalCreation with EvalInterpretation

object EvalEffect extends EvalEffect
