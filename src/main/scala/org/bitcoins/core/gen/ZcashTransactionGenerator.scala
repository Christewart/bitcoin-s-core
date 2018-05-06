package org.bitcoins.core.gen

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{ BaseTransaction, ZcashTransaction, ZcashTxConstants }
import org.scalacheck.Gen

sealed abstract class ZcashTransactionGenerator {

  /** Generates txs that are valid before the overwinter network transition */
  def preOverWinterTx: Gen[ZcashTransaction] = for {
    is <- TransactionGenerators.smallInputs
    os <- TransactionGenerators.smallOutputs
    lockTime <- NumberGenerator.uInt32s
  } yield ZcashTransaction(ZcashTxConstants.version, is, os, lockTime).get

  def transaction: Gen[ZcashTransaction] = preOverWinterTx
}

object ZcashTransactionGenerator extends ZcashTransactionGenerator