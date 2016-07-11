package org.atnos.eff

import Effects._
import cats.data.Xor
import scala.annotation.implicitNotFound

/**
 * Member typeclass for effects belonging to a stack of effects R
 *
 * If T is a member of R then we can:
 *
 * - create a Union of effects from a single effect with "inject"
 * - extract an effect value from a union if there is such an effect in the stack
 */
@implicitNotFound("No instance found for Member[${T}, ${R}]. The ${T} effect is not part of the stack ${R} or it was not possible to determine the stack that would result from removing ${T} from ${R}")
trait Member[T[_], R] {
  type Out

  def inject[V](tv: T[V]): Union[R, V]

  def accept[V](union: Union[Out, V]): Union[R, V]

  def project[V](union: Union[R, V]): Union[Out, V] Xor T[V]

  def aux: Member.Aux[T, R, Out] =
    this

  /** @return a member instance for another effect in the remaining stack */
  def out[W[_]](implicit w: Member[W, R]): Member[W, Out]
}

object Member extends MemberImplicits {

  @implicitNotFound("No instance found for Member[${T}, ${R}]. The ${T} effect is not part of the stack ${R} or it was not possible to determine the stack that would result from removing ${T} from ${R}")
  type Aux[T[_], R, U] = Member[T, R] { type Out = U }

  @implicitNotFound("No instance found for Member[${T}, ${R}]. The ${T} effect is not part of the stack ${R} or it was not possible to determine the stack that would result from removing ${T} from ${R}")
  type <=[T[_], R] = Member[T, R]

  def apply[T[_], R](implicit m: Member[T, R]): Member[T, R] =
    m

  def aux[T[_], R, U](implicit m: Member.Aux[T, R, U]): Member.Aux[T, R, U] =
    m

  def unaux[T[_], R, U](implicit m: Member.Aux[T, R, U]): Member[T, R] =
    m

  def ZeroMember[T[_], R]: Member.Aux[T, T |: R, R] = new Member[T, T |: R] {
    type Out = R

    def inject[V](effect: T[V]): Union[T |: R, V] =
      Union.now(effect)

    def accept[V](union: Union[Out, V]): Union[T |: R, V] =
      UnionNext(union)

    def project[V](union: Union[T |: R, V]): Union[R, V] Xor T[V] =
      union match {
        case UnionNow(x) => Xor.Right(x)
        case UnionNext(u@UnionNow(x)) => Xor.Left(UnionNow(x).asInstanceOf[Union[R, V]])
        case UnionNext(u@UnionNext(x)) => Xor.Left(UnionNext(x).asInstanceOf[Union[R, V]])
      }

    def out[W[_]](implicit w: Member[W, T |: R]): Member[W, Out] = new Member[W, R] {
      type Out = Any

      def inject[V](effect: W[V]) =
        w.inject[V](effect) match {
          case UnionNow(x)  => sys.error("can not happen, W is not the first effect of T |: R")
          case UnionNext(u) => u.asInstanceOf[Union[R, V]]
        }

      def accept[V](union: Union[Out, V]): Union[R, V] =
        sys.error("this method should not be used - this is a member instance used for inject only")

      def project[V](union: Union[R, V]): Union[R, V] Xor W[V] =
        sys.error("this method should not be used - this is a member instance used for inject only")

      def out[W1[_]](implicit w: Member[W1, R]): Member[W1, Out] =
        sys.error("this method should not be used - this is a member instance used for inject only")
    }
  }

  def SuccessorMember[T[_], O[_], R, U](implicit m: Member.Aux[T, R, U]): Member.Aux[T, O |: R, O |: U] = new Member[T, O |: R] {
    type Out = O |: U

    def inject[V](effect: T[V]) =
      Union.next(m.inject[V](effect))

    def accept[V](union: Union[Out, V]): Union[O |: R, V] =
      union match {
        case UnionNow(x)  => union.asInstanceOf[Union[O |: R, V]]
        case UnionNext(u) => UnionNext(m.accept(u.asInstanceOf[Union[U, V]]))
      }

    def project[V](union: Union[O |: R, V]): Union[Out, V] Xor T[V] =
      union match {
        case UnionNow(x) => Xor.left(UnionNow(x).asInstanceOf[Union[Out, V]])
        case UnionNext(u) => m.project[V](u).leftMap(u1 => UnionNext(u1).asInstanceOf[Union[Out, V]])
      }

    def out[W[_]](implicit w: Member[W, O |: R]): Member[W, O |: U] = new Member[W, O |: U] {
      type Out = Any

      def inject[V](effect: W[V]) =
        w.inject[V](effect) match {
          case UnionNow(x)  => sys.error("can not happen, W is not the first effect of O |: R")
          case UnionNext(u) => u.asInstanceOf[Union[O |: U, V]]
        }

      def accept[V](union: Union[Any, V]): Union[O |: U, V] =
        sys.error("this method should not be used - this is a member instance used for inject only")

      def out[W1[_]](implicit w1: Member[W1, O |: U]): Member[W1, Any] =
        sys.error("this method should not be used - this is a member instance used for inject only")

      def project[V](union: Union[O |:U, V]): Union[Any,V] Xor W[V] =
        sys.error("this method should not be used - this is a member instance used for inject only")

    }
  }
}

trait MemberImplicits extends MemberImplicits1 {
  implicit def zero[T[_]]: Member.Aux[T, T |: NoEffect, NoEffect] =
    Member.ZeroMember[T, NoEffect]
}

trait MemberImplicits1 extends MemberImplicits2 {
  implicit def first[T[_], R]: Member.Aux[T, T |: R, R] =
    Member.ZeroMember[T, R]
}

trait MemberImplicits2 extends MemberImplicits3 {
  implicit def successor[T[_], O[_], R, U](implicit m: Member.Aux[T, R, U]): Member.Aux[T, O |: R, O |: U] =
    Member.SuccessorMember[T, O, R, U](m)
}

trait MemberImplicits3 {

  implicit def successor_[T[_], O[_], R](implicit m: Member[T, R]): Member[T, O |: R] =
    Member.SuccessorMember[T, O, R, m.Out](m)

}

