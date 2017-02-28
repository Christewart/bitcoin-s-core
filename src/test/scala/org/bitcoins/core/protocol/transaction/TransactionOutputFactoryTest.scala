package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/30/16.
 */
class TransactionOutputFactoryTest extends FlatSpec with MustMatchers {

  "TransactionOutputFactory" must "create a transaction output out of it's base components" in {
    val emptyTxOutput = TransactionOutput(EmptyTransactionOutput.value,EmptyTransactionOutput.scriptPubKey)
    emptyTxOutput.value must be (EmptyTransactionOutput.value)
    emptyTxOutput.scriptPubKey must be (EmptyTransactionOutput.scriptPubKey)
  }

  it must "modify the currency unit for a tx output" in {
    val newTxOutput = TransactionOutput(EmptyTransactionOutput,Satoshis.one)
    newTxOutput.value must be (Satoshis.one)
  }

  it must "modify the scriptPubKey for a tx output" in {
    val newTxOutput = TransactionOutput(EmptyTransactionOutput,TestUtil.scriptPubKey)
    newTxOutput.scriptPubKey must be (TestUtil.scriptPubKey)
  }
}
