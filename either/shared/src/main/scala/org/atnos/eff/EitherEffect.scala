package org.atnos.eff

/**
 * Effect for computation which can fail
 */
trait EitherEffect extends EitherCreation with EitherInterpretation

object EitherEffect extends EitherEffect
