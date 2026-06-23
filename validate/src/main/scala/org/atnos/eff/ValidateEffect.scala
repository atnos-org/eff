package org.atnos.eff

/**
 * Effect for computation which can fail but will accumulate errors
 *
 * The runValidate interpreter just collects the messages and returns them at the end
 *
 */
trait ValidateEffect extends ValidateCreation with ValidateInterpretation

object ValidateEffect extends ValidateEffect
