package org.atnos.eff

sealed trait Choose[T]
case class ChooseZero[T]() extends Choose[T]
case object ChoosePlus extends Choose[Boolean]
