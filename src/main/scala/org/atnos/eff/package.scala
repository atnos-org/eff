package org.atnos

package object eff {

  type <=[M[_], R] = Member.<=[M, R]

  object all extends
    ReaderCreation with ReaderInterpretation with
    WriterCreation with WriterInterpretation with
    StateCreation with StateInterpretation with
    EvalCreation with EvalInterpretation with 
    OptionCreation with OptionInterpretation with 
    ListCreation with ListInterpretation with
    XorCreation with XorInterpretation with
    ValidateCreation with ValidateInterpretation with
    ChooseCreation with ChooseInterpretation with
    EffCreation with EffInterpretation with
    Effects

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

  object implicits extends
    ReaderImplicits with
    WriterImplicits with
    StateImplicits with
    XorImplicits with
    ValidateImplicits with
    EffImplicits

  object interpret extends
    Interpret

}
