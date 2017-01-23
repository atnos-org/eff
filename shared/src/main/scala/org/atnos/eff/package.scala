package org.atnos

package object eff {

  type <=[M[_], R] = Member.<=[M, R]
  type /=[M[_], R] = MemberInOut./=[M, R]
  type |=[M[_], R] = MemberIn.|=[M, R]

  object eff        extends EffCreation            with EffInterpretation
  object reader     extends ReaderCreation         with ReaderInterpretation
  object writer     extends WriterCreation         with WriterInterpretation
  object state      extends StateCreation          with StateInterpretation     with StateImplicits
  object eval       extends EvalCreation           with EvalInterpretation
  object option     extends OptionCreation         with OptionInterpretation
  object list       extends ListCreation           with ListInterpretation
  object either     extends EitherCreation         with EitherInterpretation    with EitherImplicits
  object validate   extends ValidateCreation       with ValidateInterpretation
  object choose     extends ChooseCreation         with ChooseInterpretation
  object safe       extends SafeCreation           with SafeInterpretation
  object future     extends FutureCreation         with FutureInterpretation
  @deprecated("The Async effect will be removed in favor of concrete asynchronous effects, like TimedFuture.", since = "2.3.0")
  object async      extends AsyncCreation          with AsyncInterpretation
  object memo       extends MemoCreation           with MemoInterpretation
  object batch      extends Batch

  object create extends
    ReaderCreation with
    WriterCreation with
    StateCreation with
    EvalCreation with
    OptionCreation with
    ListCreation with
    EitherCreation with
    ValidateCreation with
    ChooseCreation with
    FutureCreation with
    MemoCreation with
    EffCreation with
    SafeCreation

  object all extends
    ReaderEffect with
    WriterEffect with
    StateEffect with
    EvalEffect with
    OptionEffect with
    ListEffect with
    EitherEffect with
    ValidateEffect with
    ChooseEffect with
    SafeEffect with
    MemoEffect with
    Batch with
    EffInterpretation with
    EffCreation with
    EffImplicits

  object interpret extends
    Interpret with
    Batch

}
