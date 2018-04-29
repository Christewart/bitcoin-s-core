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

sealed abstract class BitcoinFundingInfo extends FundingInfo {
  override def utxos: Seq[BitcoinUTXOSpendingInfo]
}

object BitcoinFundingInfo {
  private case class BitcoinFundingInfoImpl(
    transaction: Transaction,
    utxos: Seq[BitcoinUTXOSpendingInfo]) extends BitcoinFundingInfo
  def apply(tx: BaseTransaction, utxos: Seq[BitcoinUTXOSpendingInfo]): BitcoinFundingInfo = {
    BitcoinFundingInfoImpl(tx, utxos)
  }

  def apply(wtx: WitnessTransaction, utxos: Seq[BitcoinUTXOSpendingInfo]): BitcoinFundingInfo = {
    BitcoinFundingInfoImpl(wtx, utxos)
  }
}

sealed abstract class ZcashFundingInfo extends FundingInfo {
  override def transaction: ZcashTransaction
  override def utxos: Seq[ZcashUTXOSpendingInfo]
}

object ZcashFundingInfo {
  private case class ZcashFundingInfoImpl(
    transaction: ZcashTransaction,
    utxos: Seq[ZcashUTXOSpendingInfo]) extends ZcashFundingInfo

  def apply(transaction: ZcashTransaction, utxos: Seq[ZcashUTXOSpendingInfo]): ZcashFundingInfo = {
    ZcashFundingInfoImpl(transaction, utxos)
  }
}
