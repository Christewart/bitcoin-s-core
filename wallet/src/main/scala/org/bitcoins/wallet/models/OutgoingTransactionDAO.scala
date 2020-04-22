package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.wallet.config._
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.ExecutionContext

case class OutgoingTransactionDAO()(
    implicit val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends TxDAO[OutgoingTransactionDb] {

  import profile.api._

  override val table = TableQuery[OutgoingTransactionTable]

  class OutgoingTransactionTable(tag: Tag)
      extends TxTable[OutgoingTransactionDb](tag, "wallet_outgoing_txs") {

    import org.bitcoins.db.DbCommonsColumnMappers._

    def txIdBE: Rep[DoubleSha256DigestBE] = column("txIdBE", O.Unique)

    def inputAmount: Rep[CurrencyUnit] = column("inputAmount")

    def sentAmount: Rep[CurrencyUnit] = column("sentAmount")

    def actualFee: Rep[CurrencyUnit] = column("actualFee")

    def expectedFee: Rep[CurrencyUnit] = column("expectedFee")

    def feeRate: Rep[SatoshisPerByte] = column("feeRate")

    private type OutgoingTransactionTuple =
      (
          DoubleSha256DigestBE,
          CurrencyUnit,
          CurrencyUnit,
          CurrencyUnit,
          CurrencyUnit,
          SatoshisPerByte)

    private val fromTuple: OutgoingTransactionTuple => OutgoingTransactionDb = {
      case (txId, inputAmount, sentAmount, actualFee, expectedFee, feeRate) =>
        OutgoingTransactionDb(txId,
                              inputAmount,
                              sentAmount,
                              actualFee,
                              expectedFee,
                              feeRate)
    }

    private val toTuple: OutgoingTransactionDb => Option[
      OutgoingTransactionTuple] = tx =>
      Some(
        (tx.txIdBE,
         tx.inputAmount,
         tx.sentAmount,
         tx.actualFee,
         tx.expectedFee,
         tx.feeRate))

    def * : ProvenShape[OutgoingTransactionDb] =
      (txIdBE, inputAmount, sentAmount, actualFee, expectedFee, feeRate) <> (fromTuple, toTuple)

    def primaryKey: PrimaryKey =
      primaryKey("pk_tx", sourceColumns = txIdBE)

    def fk_underlying_tx = {
      val txTable = TransactionDAO().table
      foreignKey("fk_underlying_tx",
                 sourceColumns = txIdBE,
                 targetTableQuery = txTable)(_.txIdBE)
    }
  }
}
