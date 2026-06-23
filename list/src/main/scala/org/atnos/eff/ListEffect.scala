package org.atnos.eff

/**
 * Effect for computations possibly returning several values
 */
trait ListEffect extends ListCreation with ListInterpretation

object ListEffect extends ListEffect
