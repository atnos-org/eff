package org.atnos

package object eff {

  type <=[M[_], R] = Member.<=[M, R]
  type /=[M[_], R] = MemberInOut./=[M, R]
  type |=[M[_], R] = MemberIn.|=[M, R]

  object eff extends EffCreation with EffInterpretation
  object validate extends ValidateCreation with ValidateInterpretation
  object choose extends ChooseCreation with ChooseInterpretation
  object safe extends SafeCreation with SafeInterpretation
  object batch extends Batch
  object interpret extends Interpret with Batch

}
