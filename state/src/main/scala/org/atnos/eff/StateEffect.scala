package org.atnos.eff

/**
 * Effect for passing state along computations
 *
 * Internally backed up by cats.data.State
 *
 */
trait StateEffect extends StateCreation with StateInterpretation

object StateEffect extends StateEffect
