package org.atnos

package object eff {

  object all extends
    ReaderCreation with ReaderInterpretation with
    WriterCreation with WriterInterpretation with
    StateCreation with StateInterpretation with
    EvalCreation with EvalInterpretation with 
    OptionCreation with OptionInterpretation with 
    ListCreation with ListInterpretation with 
    DisjunctionCreation with DisjunctionInterpretation with 
    ChooseCreation with ChooseInterpretation with
    EffCreation with EffInterpretation with
    Effects

  object implicits extends
    ReaderImplicits with
    WriterImplicits with
    StateImplicits with
    DisjunctionImplicits with
    EffImplicits

}
