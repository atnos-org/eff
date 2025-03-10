package org.atnos.eff

sealed abstract class Validate[+E, A]
case class Correct[E]() extends Validate[E, Unit]
case class Warning[E](e: E) extends Validate[E, Unit]
case class Wrong[E](e: E) extends Validate[E, Unit]
