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
    EitherCreation with EitherInterpretation with
    ValidateCreation with ValidateInterpretation with
    ChooseCreation with ChooseInterpretation with
    EffCreation with EffInterpretation with
    Effects

  object implicits extends
    ReaderImplicits with
    WriterImplicits with
    StateImplicits with
    EitherImplicits with
    ValidateImplicits with
    EffImplicits

  object interpret extends
    Interpret

}
