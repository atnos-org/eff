package org.atnos.eff

/** list of Member instances for a given stack R */
sealed trait Members

object Members {

  type &:[H, T <: Members] = Cons[H, T]
  type &&:[H1, H2] = Cons[H1, Cons[H2, NoMember]]

  implicit def extractMember[T <: Members, H[_], Op[_[_], _], R](implicit effects: T, extract: ExtractMember[T, Op[H, R]]): Op[H, R] =
    extract.member(effects)
}

case class Cons[H, T <: Members](head: H, tail: T) extends Members

object Cons extends ConsLower1 {

  implicit def tailEffect[H[_], Op[_[_], _], R](implicit h: Op[H, R]): Cons[Op[H, R], NoMember] =
    Cons(h, NoMember())

}

trait ConsLower1 {
  implicit def headEffect[H[_], Op[_[_], _], R, T <: Members](implicit t: T, h: Op[H, R]): Cons[Op[H, R], T] =
    Cons(h, t)
}

case class NoMember() extends Members

/**
 * Type class to extract members from a list of Member instances
 */
trait ExtractMember[T, +H] {
  def member(t: T): H
}

object ExtractMember extends ExtractLower1 {
  implicit def extractHead[H, T <: Members]: ExtractMember[Cons[H, T], H] =
    _.head
}

trait ExtractLower1 {
  implicit def extractTail[H1, H2, T <: Members](implicit extract: ExtractMember[T, H2]): ExtractMember[Cons[H1, T], H2] =
    effects => extract.member(effects.tail)
}
