package org.atnos

package object eff {

  type <=[M[_], R] = Member.<=[M, R]

  object eff extends EffCreation with EffInterpretation with Effects
  object reader extends ReaderCreation with ReaderInterpretation
  object writer extends WriterCreation with WriterInterpretation
  object state extends StateCreation with StateInterpretation
  object eval extends EvalCreation with EvalInterpretation
  object option extends OptionCreation with OptionInterpretation
  object list extends ListCreation with ListInterpretation
  object xor extends XorCreation with XorInterpretation
  object validate extends ValidateCreation with ValidateInterpretation
  object choose extends ChooseCreation with ChooseInterpretation
  object future extends FutureCreation with FutureInterpretation

  object all extends
    ReaderEffect with
    WriterEffect with
    StateEffect with
    EvalEffect with
    OptionEffect with
    ListEffect with
    XorEffect with
    ValidateEffect with
    ChooseEffect with
    FutureEffect with
    EffInterpretation with
    EffCreation with
    EffImplicits with
    Effects

  object interpret extends
    Interpret

}
