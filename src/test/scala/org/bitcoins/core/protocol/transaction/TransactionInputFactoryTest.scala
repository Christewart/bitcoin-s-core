package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/30/16.
 */
class TransactionInputFactoryTest extends FlatSpec with MustMatchers {

  "TransactionInputFactory" must "create a transaction input out of it's base components" in {

    val input = TransactionInput(EmptyTransactionInput.previousOutput,EmptyTransactionInput.scriptSignature, EmptyTransactionInput.sequence)
    input.previousOutput must be (EmptyTransactionInput.previousOutput)
    input.scriptSignature must be (EmptyTransactionInput.scriptSignature)
    input.sequence must be (EmptyTransactionInput.sequence)
  }

  it must "modify an input from an output and that output's transaction" in {
    val input = TestUtil.txInput.head
    val newInput = TransactionInput(input,TestUtil.simpleTransaction.outputs(0), TestUtil.simpleTransaction)

    newInput.previousOutput.txId must be (TestUtil.simpleTransaction.txId)
    newInput.previousOutput.vout must be (UInt32.zero)
  }

  it must "chage the input's outpoint to the given outpoint" in {
    val input = TestUtil.txInput.head
    val newInput = TransactionInput(input,EmptyTransactionOutPoint)
    newInput.previousOutput must be (EmptyTransactionOutPoint)
  }

  it must "create a coinbase input if a we give an empty transaction outpoint" in {
    val input = TransactionInput(EmptyTransactionOutPoint,ScriptSignature.empty, TransactionConstants.sequence)
    input.isInstanceOf[CoinbaseInput] must be (true)
  }
}
