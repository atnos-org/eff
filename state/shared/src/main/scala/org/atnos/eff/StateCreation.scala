package org.atnos.eff

import Eff._
import cats._
import data._

trait StateCreation {

  /** store a new state value */
  def put[R, S](s: S)(implicit member: State[S, *] |= R): Eff[R, Unit] =
    send[State[S, *], R, Unit](State.set(s))

  /** get the current state value */
  def get[R, S](implicit member: State[S, *] |= R): Eff[R, S] =
    send[State[S, *], R, S](State.get)

  /** get the current state value and map it with a function f */
  def gets[R, S, T](f: S => T)(implicit member: State[S, *] |= R): Eff[R, T] =
    send[State[S, *], R, T](State.inspect(f))

  /** modify the current state value */
  def modify[R, S](f: S => S)(implicit member: State[S, *] |= R): Eff[R, Unit] =
    send[State[S, *], R, Unit](State.modify(f))

}

object StateCreation extends StateCreation
