package org.atnos.eff

/**
 * The Choose effect models non-determinism
 * So we can get results, either:
 *   - no results (when using ChooseZero)
 *   - the result for action1 or the result for action b (when using ChoosePlus)
 *
 * When running this effect we can "collect" the results with any
 * F which has an Alternative instance.
 *
 * For example if F is List then:
 *  - no results is the empty list
 *  - the result for a or b is List(a, b)
 *
 * If F is Option then:
 *  - no results is the None
 *  - the result for a or b is Some(a) or Some(b
 */
trait ChooseEffect extends ChooseCreation with ChooseInterpretation

object ChooseEffect extends ChooseEffect
