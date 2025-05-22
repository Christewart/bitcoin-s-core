package org.bitcoins.core.script.arithmetic
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptWitness,
  TaprootScriptPath
}
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.script.bitwise.{OP_EQUAL, OP_EQUALVERIFY}
import org.bitcoins.core.script.constant.*
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.{ScriptErrorEvalFalse, ScriptOk}
import org.bitcoins.core.script.stack.{OP_DUP, OP_SWAP}
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
    val script = List(OP_IN_AMOUNT,
                      OP_SWAP,
                      OP_OUT_AMOUNT,
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
                                  Vector(witness),
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))
    val result = ScriptInterpreter.run(program)
    assert(result == ScriptOk)
  }

  it must "fail if we attempt to withdraw more than 1 BTC" in {
    val script = List(OP_IN_AMOUNT,
                      OP_SWAP,
                      OP_OUT_AMOUNT,
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
                                  Vector(witness),
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))
    val result = ScriptInterpreter.run(program)
    assert(result == ScriptErrorEvalFalse)
  }

  it must "enforce a maximum fee" in {
    val MAX_FEE = Satoshis(100_000)
    val MAX_FEE_SN = ScriptNumber(MAX_FEE.toLong)
    val PUSH_MAX_FEE = BytesToPushOntoStack(MAX_FEE_SN.bytes.size)
    val script: List[ScriptToken] = List(OP_IN_AMOUNT,
                                         OP_SWAP,
                                         OP_OUT_AMOUNT,
                                         OP_SUB,
                                         PUSH_MAX_FEE,
                                         MAX_FEE_SN,
                                         OP_LESSTHANOREQUAL)
    val witnessStack = Vector(ScriptNumber(7), ScriptNumber.one)
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
      TestUtil.testTaprootProgram(
        taprootSPK,
        Vector(witness, ScriptWitness.empty, ScriptWitness.empty),
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
      TestUtil.testTaprootProgram(
        taprootSPK,
        Vector(witness, ScriptWitness.empty, ScriptWitness.empty),
        fundingOutputsOpt = Some(fundingOutputs),
        spendingOutputsOpt = Some(invaldSpendingOutputs)
      )
    val result1 = ScriptInterpreter.run(program1)
    assert(result1 == ScriptErrorEvalFalse)
  }

  it must "handle OP_0 correctly" in {
    val script: List[ScriptToken] =
      List(OP_OUT_AMOUNT, OP_IN_AMOUNT, OP_0, OP_EQUALVERIFY, OP_0, OP_EQUAL)

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
      TestUtil.testTaprootProgram(
        taprootSPK,
        Vector(witness, ScriptWitness.empty, ScriptWitness.empty),
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
      TestUtil.testTaprootProgram(
        taprootSPK,
        Vector(witness, ScriptWitness.empty, ScriptWitness.empty),
        fundingOutputsOpt = Some(fundingOutputs),
        spendingOutputsOpt = Some(spendingOutputs))
    val result0 = ScriptInterpreter.run(program0)
    assert(result0 == ScriptOk)
  }

  it must "showcase transaction malleability" in {
    val script = List(OP_1,
                      OP_OUT_AMOUNT,
                      OP_SWAP,
                      OP_IN_AMOUNT,
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
                                  Vector(witness, ScriptWitness.empty),
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
                                  Vector(witness),
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))
    val result0 = ScriptInterpreter.run(program0)
    assert(result0 == ScriptOk)
  }

  it must "map 2 inputs to the same output" in {
    val script = List(OP_1,
                      OP_OUT_AMOUNT,
                      OP_SWAP,
                      OP_IN_AMOUNT,
                      PUSH_ONE_BTC,
                      ONE_BTC,
                      OP_DUP,
                      OP_EQUALVERIFY,
                      OP_EQUAL)
    val witnessStack = Vector(ScriptNumber.one)
      .map(_.bytes)
    val (taprootSPK0, witnessNoStack0: TaprootScriptPath) =
      TransactionTestUtil.buildTaprootSPK(script)
    val (taprootSPK1, witnessNoStack1) =
      TransactionTestUtil.buildTaprootSPK(script)
    val witness0 = witnessNoStack0.copy(witnessNoStack0.stack ++ witnessStack)
    val witness1 = witnessNoStack1.copy(witnessNoStack1.stack ++ witnessStack)
    val fundingOutputs =
      Vector(TransactionOutput(Bitcoins.one, taprootSPK0),
             TransactionOutput(Bitcoins.one, taprootSPK1))
    val spendingOutputs =
      Vector(TransactionOutput(Bitcoins.one, ScriptPubKey.empty))

    val program0 =
      TestUtil.testTaprootProgram(taprootSPK0,
                                  Vector(witness0, witness1),
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs))

    val result0 = ScriptInterpreter.run(program0)
    assert(result0 == ScriptOk)

    val program1 =
      TestUtil.testTaprootProgram(taprootSPK1,
                                  Vector(witness0, witness1),
                                  fundingOutputsOpt = Some(fundingOutputs),
                                  spendingOutputsOpt = Some(spendingOutputs),
                                  inputIndex = UInt32.one)
    val result1 = ScriptInterpreter.run(program1)
    assert(result1 == ScriptOk)
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
    idxOpWPushOp ++ Vector(
      OP_OUT_AMOUNT,
      pushAmt,
      amtScriptNumber,
      OP_EQUALVERIFY
    )
  }

}
