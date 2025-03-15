package org.bitcoins.core.script.arithmetic
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.script.{ScriptPubKey, TaprootScriptPath}
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant.*
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.{ScriptErrorEvalFalse, ScriptOk}
import org.bitcoins.core.script.stack.{OP_DROP, OP_DUP}
import org.bitcoins.testkitcore.util.{
  BitcoinSUnitTest,
  TestUtil,
  TransactionTestUtil
}

class InOutAmountTest extends BitcoinSUnitTest {

  behavior of "InOutAmount"

  val ONE_BTC = ScriptNumber(Bitcoins.one.satoshis.toLong)
  val PUSH_ONE_BTC = BytesToPushOntoStack(ONE_BTC.bytes.size)
  it must "only allow withdrawing 1 BTC" in {
    val script = List(OP_INOUT_AMOUNT,
                      OP_SUB,
                      PUSH_ONE_BTC,
                      ONE_BTC,
                      OP_GREATERTHANOREQUAL)
    val witnessStack = Vector(ScriptNumber.one, ScriptNumber.one)
      .map(_.bytes)
    val (taprootSPK, witnessNoStack: TaprootScriptPath) =
      TransactionTestUtil.buildTaprootSPK(script)
    val witness = witnessNoStack.copy(witnessNoStack.stack ++ witnessStack)
    val fundingOutputs = Vector(TransactionOutput(Bitcoins.two, taprootSPK))
    val spendingOutputs =
      Vector(TransactionOutput(Bitcoins.one, ScriptPubKey.empty))
    val program =
      TestUtil.testTaprootProgram(taprootSPK,
                                  witness,
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))
    val result = ScriptInterpreter.run(program)
    assert(result == ScriptOk)
  }

  it must "fail if we attempt to withdraw more than 1 BTC" in {
    val script = List(OP_INOUT_AMOUNT,
                      OP_SUB,
                      PUSH_ONE_BTC,
                      ONE_BTC,
                      OP_GREATERTHANOREQUAL)
    val witnessStack = Vector(ScriptNumber.one, ScriptNumber.one)
      .map(_.bytes)
    val (taprootSPK, witnessNoStack: TaprootScriptPath) =
      TransactionTestUtil.buildTaprootSPK(script)
    val witness = witnessNoStack.copy(witnessNoStack.stack ++ witnessStack)
    val fundingOutputs = Vector(TransactionOutput(Bitcoins.two, taprootSPK))
    val spendingOutputs =
      Vector(TransactionOutput(Bitcoins.one + Satoshis.one, ScriptPubKey.empty))
    val program =
      TestUtil.testTaprootProgram(taprootSPK,
                                  witness,
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))
    val result = ScriptInterpreter.run(program)
    assert(result == ScriptErrorEvalFalse)
  }

  it must "enforce a maximum fee" in {
    val MAX_FEE = Satoshis(100_000)
    val MAX_FEE_SN = ScriptNumber(MAX_FEE.toLong)
    val PUSH_MAX_FEE = BytesToPushOntoStack(MAX_FEE_SN.bytes.size)
    val script: List[ScriptToken] = List(OP_INOUT_AMOUNT,
                                         OP_SUB,
                                         PUSH_MAX_FEE,
                                         MAX_FEE_SN,
                                         OP_LESSTHANOREQUAL)
    val witnessStack = Vector(ScriptNumber.one, ScriptNumber(7))
      .map(_.bytes)
    val (taprootSPK, witnessNoStack: TaprootScriptPath) =
      TransactionTestUtil.buildTaprootSPK(script)

    val witness = witnessNoStack.copy(witnessNoStack.stack ++ witnessStack)
    // fund the transaction with 6BTC
    val fundingOutputs =
      Vector(TransactionOutput(Bitcoins.two, taprootSPK),
             TransactionOutput(Bitcoins.one, ScriptPubKey.empty),
             TransactionOutput(Bitcoins(3), ScriptPubKey.empty))

    val spendingOutputs =
      Vector(TransactionOutput(Bitcoins(6), ScriptPubKey.empty))

    val program0 =
      TestUtil.testTaprootProgram(taprootSPK,
                                  witness,
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))
    val result0 = ScriptInterpreter.run(program0)
    assert(result0 == ScriptOk)

    // run test again with with one satoshi deducted from the spending outputs
    val invaldSpendingOutputs =
      Vector(
        TransactionOutput(Bitcoins(6) - MAX_FEE - Satoshis.one,
                          ScriptPubKey.empty))

    val program1 =
      TestUtil.testTaprootProgram(taprootSPK,
                                  witness,
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt =
                                    Some(invaldSpendingOutputs))
    val result1 = ScriptInterpreter.run(program1)
    assert(result1 == ScriptErrorEvalFalse)
  }

  it must "handle OP_0 correctly" in {
    val script: List[ScriptToken] =
      List(OP_INOUT_AMOUNT, OP_0, OP_EQUALVERIFY, OP_0, OP_EQUAL)

    val witnessStack = Vector(ScriptNumber.zero, ScriptNumber.zero)
      .map(_.bytes)
    val (taprootSPK, witnessNoStack: TaprootScriptPath) =
      TransactionTestUtil.buildTaprootSPK(script)

    val witness = witnessNoStack.copy(witnessNoStack.stack ++ witnessStack)
    // fund the transaction with 6BTC
    val fundingOutputs =
      Vector(TransactionOutput(Bitcoins.two, taprootSPK),
             TransactionOutput(Bitcoins.one, ScriptPubKey.empty),
             TransactionOutput(Bitcoins(3), ScriptPubKey.empty))

    val spendingOutputs =
      Vector(TransactionOutput(Bitcoins(6), ScriptPubKey.empty))

    val program0 =
      TestUtil.testTaprootProgram(taprootSPK,
                                  witness,
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))
    val result0 = ScriptInterpreter.run(program0)
    assert(result0 == ScriptOk)
  }

  it must "enforce uniform output values" in {
    val script: List[ScriptToken] = {
      generateUniformOutputForIdx(0, Bitcoins.one) ++
        generateUniformOutputForIdx(1, Bitcoins.one) ++
        Vector(OP_1)
    }.toList

    val witnessStack = Vector.empty
    val (taprootSPK, witnessNoStack: TaprootScriptPath) =
      TransactionTestUtil.buildTaprootSPK(script)

    val witness = witnessNoStack.copy(witnessNoStack.stack ++ witnessStack)
    // fund the transaction with 6BTC
    val fundingOutputs =
      Vector(TransactionOutput(Bitcoins.two, taprootSPK),
             TransactionOutput(Bitcoins.one, ScriptPubKey.empty),
             TransactionOutput(Bitcoins(3), ScriptPubKey.empty))

    val spendingOutputs =
      Vector(TransactionOutput(Bitcoins.one, ScriptPubKey.empty),
             TransactionOutput(Bitcoins.one, ScriptPubKey.empty))

    val program0 =
      TestUtil.testTaprootProgram(taprootSPK,
                                  witness,
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))
    val result0 = ScriptInterpreter.run(program0)
    assert(result0 == ScriptOk)
  }

  it must "showcase transaction malleability" in {
    val script = List(OP_1,
                      OP_INOUT_AMOUNT,
                      PUSH_ONE_BTC,
                      ONE_BTC,
                      OP_DUP,
                      OP_EQUALVERIFY,
                      OP_EQUAL)
    val witnessStack = Vector(ScriptNumber.one)
      .map(_.bytes)
    val (taprootSPK, witnessNoStack: TaprootScriptPath) =
      TransactionTestUtil.buildTaprootSPK(script)
    val witness = witnessNoStack.copy(witnessNoStack.stack ++ witnessStack)
    val fundingOutputs =
      Vector(TransactionOutput(Bitcoins.one, taprootSPK),
             TransactionOutput(Bitcoins.one, ScriptPubKey.empty))
    val spendingOutputs =
      Vector(TransactionOutput(Bitcoins.one, ScriptPubKey.empty))
    val program =
      TestUtil.testTaprootProgram(taprootSPK,
                                  witness,
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))
    val result = ScriptInterpreter.run(program)
    assert(result == ScriptOk)
  }

  it must "enforce uniform amounts with different spending paths" in {
    val script: List[ScriptToken] = {
      generateUniformOutputForIdx(0, Bitcoins.one) ++
        generateUniformOutputForIdx(1, Bitcoins.one) ++
        generateUniformOutputForIdx(2, Bitcoins.one) ++
        generateUniformOutputForIdx(3, Bitcoins.one) ++
        generateUniformOutputForIdx(4, Bitcoins.one) ++
        Vector(OP_1)
    }.toList
    println(s"script=$script")

    val witnessStack = Vector.empty
    val (taprootSPK, witnessNoStack: TaprootScriptPath) =
      TransactionTestUtil.buildTaprootSPK(script)

    val witness = witnessNoStack.copy(witnessNoStack.stack ++ witnessStack)
    // fund the transaction with 6BTC
    val fundingOutputs =
      Vector(TransactionOutput(Bitcoins(2.1), taprootSPK),
             TransactionOutput(Bitcoins(3), ScriptPubKey.empty))

    val spendingOutputs =
      Vector(
        TransactionOutput(Bitcoins.one, ScriptPubKey.empty),
        TransactionOutput(Bitcoins.one, ScriptPubKey.empty),
        TransactionOutput(Bitcoins.one, ScriptPubKey.empty),
        TransactionOutput(Bitcoins.one, ScriptPubKey.empty),
        TransactionOutput(Bitcoins.one, ScriptPubKey.empty),
        TransactionOutput(Bitcoins(0.1), ScriptPubKey.empty)
      )

    val program0 =
      TestUtil.testTaprootProgram(taprootSPK,
                                  witness,
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))
    val result0 = ScriptInterpreter.run(program0)
    assert(result0 == ScriptOk)
  }

  private def generateUniformOutputForIdx(
      outputIdx: Int,
      amt: CurrencyUnit): Vector[ScriptToken] = {
    val amtScriptNumber = ScriptNumber(amt.satoshis.toLong)
    val pushAmt = BytesToPushOntoStack(ONE_BTC.bytes.size)
    val idx = 1 << outputIdx
    val idxOpWPushOp: Vector[ScriptToken] = if (idx >= 0 && idx <= 16) {
      Vector(ScriptNumberOperation.fromNumber(idx).get)
    } else {
      val num = ScriptNumber(idx)
      Vector(BytesToPushOntoStack(num.byteSize), num)
    }
    Vector(OP_0) ++ idxOpWPushOp ++ Vector(
      OP_INOUT_AMOUNT,
      pushAmt,
      amtScriptNumber,
      OP_EQUALVERIFY,
      OP_DROP // drop input amount as its not relevant
    )
  }

}
