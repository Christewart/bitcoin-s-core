package org.bitcoins.core.script.arithmetic
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.testkitcore.util.{
  BitcoinSUnitTest,
  TestUtil,
  TransactionTestUtil
}

class InOutAmountTest extends BitcoinSUnitTest {

  behavior of "InOutAmount"

  val ONE_BTC = ScriptNumber(Bitcoins.one.satoshis.toLong)
  it must "only allow withdrawing 1 BTC" in {
    // safe to use EmptyTransactionOutPoint because non-taproot
    val script = List(OP_INOUT_AMOUNT, OP_SUB, ONE_BTC, OP_GREATERTHANOREQUAL)
    val witnessStack = Vector(ScriptNumber.one, ScriptNumber.one)
      .map(_.bytes)
    val (taprootSPK, witnessNoStack) =
      TransactionTestUtil.buildTaprootSPK(script)
    val witness = witnessNoStack.copy(witnessNoStack.stack.head +: witnessStack)
    val fundingOutputs = Vector(TransactionOutput(Bitcoins.two, taprootSPK))
    val program =
      TestUtil.testTaprootProgram(taprootSPK,
                                  witness,
                                  fundingOutputsOpt = Some(fundingOutputs))
    val result = ScriptInterpreter.run(program)
    assert(result == ScriptOk)
  }

}
