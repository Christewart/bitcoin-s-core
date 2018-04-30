package org.bitcoins.core.wallet.signer

import org.bitcoins.core.protocol.transaction.{ BaseTransaction, Transaction, WitnessTransaction, ZcashTransaction }
import org.bitcoins.core.wallet.utxo.{ BitcoinUTXOSpendingInfo, UTXOSpendingInfo, ZcashUTXOSpendingInfo }

/**
 * This meant to represent the class used to 'fund' an
 * unsigned [[Transaction]].
 * This is useful for when we have multiple [[org.bitcoins.core.config.NetworkParameters]]
 * that each have their own transaction type. I.e. we should only be able to have
 * BitcoinTransactions paired with [[BitcoinUTXOSpendingInfo]], the same would apply for litecoin etc.
 */
sealed abstract class FundingInfo {
  /** The transaction we are funding with the utxos */
  def transaction: Transaction
  /** The utxos used to fund the tx */
  def utxos: Seq[UTXOSpendingInfo]
}

/** Funding information for a bitcoin transaction */
case class BitcoinFundingInfo(transaction: Transaction, utxos: Seq[BitcoinUTXOSpendingInfo]) extends FundingInfo

/** Funding information for a zcash transaction */
case class ZcashFundingInfo(transaction: ZcashTransaction, utxos: Seq[ZcashUTXOSpendingInfo]) extends FundingInfo
