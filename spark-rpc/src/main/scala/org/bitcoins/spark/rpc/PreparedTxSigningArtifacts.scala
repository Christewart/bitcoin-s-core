package org.bitcoins.spark.rpc
import org.bitcoins.core.crypto.{
  TaprootSerializationOptions,
  TaprootTxSigComponent,
  TransactionSignatureSerializer
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  WitnessTransaction
}
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.crypto.HashType
import org.bitcoins.crypto.frost.{FrostNoncePriv, FrostNoncePub}
import org.bitcoins.spark.rpc.proto.spark.SigningJob
import scodec.bits.ByteVector

case class PreparedTxSigningArtifacts(
    rawTx: ByteVector,
    fundingTx: Transaction,
    voutIdx: Int,
    nonce: FrostNoncePriv,
    job: SigningJob) {
  def tx: WitnessTransaction = WitnessTransaction(rawTx)

  def sighash: ByteVector = {
    val outpoint = TransactionOutPoint(fundingTx.txId, UInt32(voutIdx))
    val previousOutputMap = PreviousOutputMap(
      Map(outpoint -> fundingTx.outputs(voutIdx)))
    val txSigComp = TaprootTxSigComponent(tx,
                                          UInt32.zero,
                                          previousOutputMap,
                                          Policy.standardScriptVerifyFlags)
    TransactionSignatureSerializer.serializeForSignature(
      txSigComp,
      HashType.sigHashAll,
      TaprootSerializationOptions.empty)
  }
  def commitment: FrostNoncePub = nonce.toNoncePub
}
