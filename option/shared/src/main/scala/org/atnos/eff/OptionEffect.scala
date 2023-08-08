package org.atnos.eff

/**
 * Effect for optional computations
 */
trait OptionEffect extends OptionCreation with OptionInterpretation

object OptionEffect extends OptionEffect
