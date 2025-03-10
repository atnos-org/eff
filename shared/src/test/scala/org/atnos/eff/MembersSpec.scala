package org.atnos.eff

import org.specs2.Specification

class MembersSpec extends Specification {
  def is = s2"""

 Some signature can be repetitive on some methods using effects (see #56):

   def foo[R :_foo :_bar :_baz](i: Int): Eff[R, Int]

 It is possible to "pack" them with the following type definition:

   type _effects = _foo &: _bar &&: _baz

   def foo[R :_effects](i: Int): Eff[R, Int] =
     Eff.pure(1)

"""

  def packed = ok

  import Members.&:
  import Members.&&:

  trait Foo[A]
  trait Bar[A]
  trait Baz[A]
  trait Boo[A]

  type _foo[R] = Foo |= R
  type _bar[R] = Bar /= R
  type _baz[R] = Baz <= R
  type _boo[R] = Boo |= R

  case class TwoEffects() {
    type _effects[R] = _foo[R] &&: _bar[R]

    def foo[R](using _effects[R]): Eff[R, Int] = {
      import Members.given

      getFoo[R]
      getBar[R]
    }

    type S = Fx.fx2[Foo, Bar]

    foo[S]

  }

  case class ThreeEffects() {
    type _effects[R] = _foo[R] &: _bar[R] &&: _baz[R]

    def foo[R](i: Int)(using _effects[R]): Eff[R, Int] = {
      import Members.given

      getFoo[R]
      getBar[R]
      getBaz[R]
    }

    type S = Fx.fx3[Foo, Bar, Baz]
    type U = Fx.fx4[Foo, Bar, Baz, Boo]

    foo[S](1)
    foo[U](1)

  }

  def getFoo[R: _foo: _bar]: Eff[R, Int] = Eff.pure(1)
  def getBar[R: _bar]: Eff[R, Int] = Eff.pure(1)
  def getBaz[R: _baz]: Eff[R, Int] = Eff.pure(1)

}
