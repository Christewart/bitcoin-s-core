package org.bitcoins.core.protocol.dlc.execution

import org.bitcoins.core.protocol.dlc.models.OracleOutcome
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto.ECAdaptorSignature

case class SetupDLC(
    fundingTx: Transaction,
    cets: Vector[(OracleOutcome, CETInfo)],
    refundTx: WitnessTransaction) {
  cets.foreach { case (msg, cetInfo) =>
    require(
      cetInfo.tx.inputs.size == 1,
      s"CETs should only spend the funding input, local CET for $msg has ${cetInfo.tx.inputs.size} inputs")
    require(
      cetInfo.tx.inputs.head.previousOutput.txId == fundingTx.txId,
      s"CET is not spending the funding tx, ${cetInfo.tx.inputs.head}"
    )
  }
  require(
    refundTx.inputs.size == 1,
    s"RefundTx should only spend the funding input, refundTx has ${refundTx.inputs.size} inputs")
  require(
    refundTx.inputs.head.previousOutput.txId == fundingTx.txId,
    s"RefundTx is not spending the funding tx, ${refundTx.inputs.head}"
  )

  def getCETInfo(outcome: OracleOutcome): CETInfo = {
    cets.find(_._1 == outcome) match {
      case Some((_, info)) => info
      case None =>
        throw new IllegalArgumentException(
          s"No CET found for the given outcome $outcome")
    }
  }
}

case class CETInfo(tx: WitnessTransaction, remoteSignature: ECAdaptorSignature)
