package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.serializers.transaction.{RawBaseTransactionParser, RawWitnessTransactionParser}
import org.bitcoins.crypto._
import scodec.bits.ByteVector

/**
  * Created by chris on 7/14/15.
  */
sealed abstract class Transaction extends NetworkElement {

  /**
    * The `sha256(sha256(tx))` of this transaction,
    * Note that this is the little endian encoding of the hash, NOT the big endian encoding shown in block
    * explorers. See
    * [[https://bitcoin.stackexchange.com/questions/2063/why-does-the-bitcoin-protocol-use-the-little-endian-notation this link]]
    * for more info
    */
  def txId: DoubleSha256Digest = CryptoUtil.doubleSHA256(bytes)

  /**
    * This is the BIG ENDIAN encoding for the txid. This is commonly used for
    * RPC interfaces and block explorers, this encoding is NOT used at the protocol level
    * For more info see:
    * [[https://bitcoin.stackexchange.com/questions/2063/why-does-the-bitcoin-protocol-use-the-little-endian-notation]]
    */
  def txIdBE: DoubleSha256DigestBE = txId.flip

  /** The version number for this transaction */
  def version: Int32

  /** The inputs for this transaction */
  def inputs: Seq[TransactionInput]

  /** The outputs for this transaction */
  def outputs: Seq[TransactionOutput]

  /** The locktime for this transaction */
  def lockTime: UInt32

  /**
    * This is used to indicate how 'expensive' the transction is on the blockchain.
    * This use to be a simple calculation before segwit (BIP141). Each byte in the transaction
    * counted as 4 'weight' units. Now with segwit, the
    * [[org.bitcoins.core.protocol.transaction.TransactionWitness TransactionWitness]]
    * is counted as 1 weight unit per byte,
    * while other parts of the transaction (outputs, inputs, locktime etc) count as 4 weight units.
    * As we add more witness versions, this may be subject to change.
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#Transaction_size_calculations BIP 141]]
    * [[https://github.com/bitcoin/bitcoin/blob/5961b23898ee7c0af2626c46d5d70e80136578d3/src/consensus/validation.h#L96]]
    */
  def weight: Long

  /**
    * The transaction's virtual size
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#Transaction_size_calculations]]
    */
  def vsize: Long = Math.ceil(weight / 4.0).toLong

  /**
    * Base transaction size is the size of the transaction serialised with the witness data stripped
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#Transaction_size_calculations]]
    */
  def baseSize: Long = this match {
    case btx: BaseTransaction => btx.byteSize
    case wtx: WitnessTransaction =>
      BaseTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime).baseSize
  }

  def totalSize: Long = bytes.size

  /** Determines if this transaction is a coinbase transaction. */
  def isCoinbase: Boolean = inputs.size match {
    case 1 =>
      inputs.head match {
        case _: CoinbaseInput    => true
        case _: TransactionInput => false
      }
    case _: Int => false
  }

  /** Updates the input at the given index and returns the new transaction with that input updated */
  def updateInput(idx: Int, i: TransactionInput): Transaction = {
    val updatedInputs = inputs.updated(idx, i)
    this match {
      case _: BaseTransaction =>
        BaseTransaction(version, updatedInputs, outputs, lockTime)
      case wtx: WitnessTransaction =>
        WitnessTransaction(version,
                           updatedInputs,
                           outputs,
                           lockTime,
                           wtx.witness)
    }
  }
}

sealed abstract class BaseTransaction extends Transaction {
  override def bytes = RawBaseTransactionParser.write(this)
  override def weight = byteSize * 4

}

case object EmptyTransaction extends BaseTransaction {
  override def txId = DoubleSha256Digest.empty
  override def version = TransactionConstants.version
  override def inputs = Nil
  override def outputs = Nil
  override def lockTime = TransactionConstants.lockTime
}

sealed abstract class WitnessTransaction extends Transaction {
  require(
    inputs.length == witness.length,
    s"Must have same amount of inputs and witnesses in witness tx, inputs=${inputs.length} witnesses=${witness.length}"
  )

  /** The txId for the witness transaction from satoshi's original serialization */
  override def txId: DoubleSha256Digest = {
    val btx = BaseTransaction(version, inputs, outputs, lockTime)
    btx.txId
  }

  /**
    * The witness used to evaluate
    * [[org.bitcoins.core.protocol.script.ScriptSignature ScriptSignature]]/
    * [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]s inside of a SegWit tx.
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki BIP141]]
    */
  def witness: TransactionWitness

  /**
    * The witness transaction id as defined by
    * [[https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#transaction-id BIP141]]
    */
  def wTxId: DoubleSha256Digest = CryptoUtil.doubleSHA256(bytes)

  /** Returns the big endian encoding of the wtxid */
  def wTxIdBE: DoubleSha256DigestBE = wTxId.flip

  /**
    * Weight calculation in bitcoin for witness txs
    * [[https://github.com/bitcoin/bitcoin/blob/5961b23898ee7c0af2626c46d5d70e80136578d3/src/consensus/validation.h#L96]]
    */
  override def weight: Long = {
    val base = BaseTransaction(version, inputs, outputs, lockTime)
    base.byteSize * 3 + byteSize
  }
  override def bytes = RawWitnessTransactionParser.write(this)

  /**
    * Updates the [[org.bitcoins.core.protocol.script.ScriptWitness ScriptWitness]] at the given index and
    * returns a new [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]]
    * with it's witness vector updated
    */
  def updateWitness(idx: Int, scriptWit: ScriptWitness): WitnessTransaction = {
    val txWit = witness.updated(idx, scriptWit)
    WitnessTransaction(version, inputs, outputs, lockTime, txWit)
  }

}

object Transaction extends Factory[Transaction] {

  override def fromBytes(bytes: ByteVector): Transaction = {
    //see BIP141 for marker/flag bytes
    //https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#transaction-id
    if (bytes(4) == WitnessTransaction.marker && bytes(5) == WitnessTransaction.flag) {
      RawWitnessTransactionParser.read(bytes)
    } else {
      RawBaseTransactionParser.read(bytes)
    }
  }
}

object BaseTransaction extends Factory[BaseTransaction] {
  private case class BaseTransactionImpl(
      version: Int32,
      inputs: Seq[TransactionInput],
      outputs: Seq[TransactionOutput],
      lockTime: UInt32)
      extends BaseTransaction

  override def fromBytes(bytes: ByteVector): BaseTransaction =
    RawBaseTransactionParser.read(bytes)

  def apply(
      version: Int32,
      inputs: Seq[TransactionInput],
      outputs: Seq[TransactionOutput],
      lockTime: UInt32): BaseTransaction =
    BaseTransactionImpl(version, inputs, outputs, lockTime)
}

object WitnessTransaction extends Factory[WitnessTransaction] {
  private case class WitnessTransactionImpl(
      version: Int32,
      inputs: Seq[TransactionInput],
      outputs: Seq[TransactionOutput],
      lockTime: UInt32,
      witness: TransactionWitness)
      extends WitnessTransaction

  def apply(
      version: Int32,
      inputs: Seq[TransactionInput],
      outputs: Seq[TransactionOutput],
      lockTime: UInt32,
      witness: TransactionWitness): WitnessTransaction =
    WitnessTransactionImpl(version, inputs, outputs, lockTime, witness)

  override def fromBytes(bytes: ByteVector): WitnessTransaction =
    RawWitnessTransactionParser.read(bytes)

  def toWitnessTx(tx: Transaction): WitnessTransaction = tx match {
    case btx: BaseTransaction =>
      WitnessTransaction(btx.version,
                         btx.inputs,
                         btx.outputs,
                         btx.lockTime,
                         EmptyWitness.fromInputs(btx.inputs))
    case wtx: WitnessTransaction => wtx
  }

  val marker: Byte = 0.toByte
  val flag: Byte = 1.toByte

  /** These bytes -- at index 4 & 5 in a witness transaction -- are used to indicate a witness tx
    * @see BIP141 https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#transaction-id
    * */
  val witBytes: ByteVector = ByteVector(marker, flag)
}
