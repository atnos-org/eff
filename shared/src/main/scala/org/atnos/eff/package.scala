package org.atnos

package object eff {

  type <=[M[_], R] = Member.<=[M, R]
  type /=[M[_], R] = MemberInOut./=[M, R]
  type |=[M[_], R] = MemberIn.|=[M, R]

  object eff extends EffCreation with EffInterpretation
  object interpret extends Interpret

}
