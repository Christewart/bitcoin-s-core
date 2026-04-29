package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.protocol.script.{NonStandardScriptPubKey, ScriptPubKey}
import org.bitcoins.core.serializers.transaction.RawTransactionOutputParser
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

case class TransactionOutput(value: CurrencyUnit, scriptPubKey: ScriptPubKey)
    extends NetworkElement {
  override val bytes: ByteVector = RawTransactionOutputParser.write(this)

  override def toString: String = {
    s"TransactionOutput(value=${Bitcoins(value.satoshis)}, scriptPubKey=$scriptPubKey)"
  }
}

object EmptyTransactionOutput
    extends TransactionOutput(CurrencyUnits.negativeSatoshi,
                              ScriptPubKey.empty) {
  override def toString(): String = "EmptyTransactionOutput"
}

object TransactionOutput extends Factory[TransactionOutput] {

  def fromBytes(bytes: ByteVector): TransactionOutput =
    RawTransactionOutputParser.read(bytes)

  val ephemeralAnchor: TransactionOutput = {
    val spk =
      NonStandardScriptPubKey.fromAsmBytes(ByteVector.fromValidHex("51024e73"))
    TransactionOutput(CurrencyUnits.zero, spk)
  }
}

case class OutputWithIndex(output: TransactionOutput, index: Int)
