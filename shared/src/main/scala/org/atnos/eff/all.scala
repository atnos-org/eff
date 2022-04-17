package org.atnos.eff

object reader extends ReaderCreation with ReaderInterpretation
object writer extends WriterCreation with WriterInterpretation
object state extends StateCreation with StateInterpretation with StateImplicits
object eval extends EvalCreation with EvalInterpretation
object option extends OptionCreation with OptionInterpretation
object list extends ListCreation with ListInterpretation
object either extends EitherCreation with EitherInterpretation with EitherImplicits
object future extends FutureCreation with FutureInterpretation
object memo extends MemoCreation with MemoInterpretation

object create
    extends ReaderCreation
    with WriterCreation
    with StateCreation
    with EvalCreation
    with OptionCreation
    with ListCreation
    with EitherCreation
    with ValidateCreation
    with ChooseCreation
    with FutureCreation
    with MemoCreation
    with EffCreation
    with SafeCreation

object all
    extends ReaderEffect
    with WriterEffect
    with StateEffect
    with EvalEffect
    with OptionEffect
    with ListEffect
    with EitherEffect
    with ValidateEffect
    with ChooseEffect
    with SafeEffect
    with MemoEffect
    with Batch
    with EffInterpretation
    with EffCreation
    with EffImplicits
