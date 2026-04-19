package org.bitcoins.spark.rpc
import org.bitcoins.core.crypto.{
  TaprootSerializationOptions,
  TaprootTxSigComponent,
  TransactionSignatureSerializer
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.transaction.*
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
  def tx: WitnessTransaction = {
    val unsignedTx = Transaction(rawTx)
    unsignedTx match {
      case EmptyTransaction =>
        sys.error(s"Cannot have empty transaction as input")
      case wtx: WitnessTransaction => wtx
      case btx: BaseTransaction    =>
        // due to how we serialize transactions with no witnesses, we may need to re-add them
        // see:https://github.com/bitcoin-s/bitcoin-s/blob/1bba4a3a528e49e6e6db59a38ad78aba69bdaea8/core/src/main/scala/org/bitcoins/core/protocol/transaction/Transaction.scala#L277
        WitnessTransaction(
          version = btx.version,
          inputs = btx.inputs,
          outputs = btx.outputs,
          lockTime = btx.lockTime,
          witness = EmptyWitness.fromN(btx.inputs.size)
        )
    }
  }

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
