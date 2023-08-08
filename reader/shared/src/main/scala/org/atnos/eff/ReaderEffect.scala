package org.atnos.eff

/**
 * Effect for computations depending on an environment.
 *
 * The inside datatype for this effect is cats.data.Reader
 */
trait ReaderEffect extends ReaderCreation with ReaderInterpretation

object ReaderEffect extends ReaderEffect
