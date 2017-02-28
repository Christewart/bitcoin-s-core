package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.{CompactSizeUInt, NetworkElement}
import org.bitcoins.core.currency.{CurrencyUnits, CurrencyUnit, Satoshis}
import org.bitcoins.core.serializers.transaction.RawTransactionOutputParser

import org.bitcoins.core.protocol.script.{ScriptPubKey}
import org.bitcoins.core.util.{Factory, BitcoinSUtil}

/**
 * Created by chris on 12/26/15.
 */
sealed trait TransactionOutput extends NetworkElement {

  def value : CurrencyUnit
  def scriptPubKey : ScriptPubKey

  //https://bitcoin.org/en/developer-reference#txout
  override def size = scriptPubKey.size + 8

  override def hex = RawTransactionOutputParser.write(Seq(this))
}

case object EmptyTransactionOutput extends TransactionOutput {
  override def value = CurrencyUnits.negativeSatoshi
  override def scriptPubKey = ScriptPubKey.empty
}


case class TransactionOutputImpl(value : CurrencyUnit, scriptPubKey: ScriptPubKey) extends TransactionOutput

object TransactionOutput extends Factory[TransactionOutput] {
  def factory(oldOutput : TransactionOutput, newCurrencyUnit: CurrencyUnit) : TransactionOutput =
    TransactionOutputImpl(newCurrencyUnit,oldOutput.scriptPubKey)

  def factory(oldOutput : TransactionOutput, newScriptPubKey : ScriptPubKey) : TransactionOutput =
    TransactionOutputImpl(oldOutput.value,newScriptPubKey)

  def factory(currencyUnit: CurrencyUnit, scriptPubKey: ScriptPubKey) : TransactionOutput = TransactionOutputImpl(currencyUnit,scriptPubKey)

  //TODO: This could bomb if the transaction output is not in the right format,
  //probably should put more thought into this to make it more robust
  def fromBytes(bytes : Seq[Byte]) : TransactionOutput = RawTransactionOutputParser.read(bytes).head

  def apply(oldOutput : TransactionOutput, newCurrencyUnit: CurrencyUnit) : TransactionOutput = factory(oldOutput,newCurrencyUnit)

  def apply(oldOutput : TransactionOutput, newScriptPubKey : ScriptPubKey) : TransactionOutput = factory(oldOutput, newScriptPubKey)

  def apply(currencyUnit: CurrencyUnit, scriptPubKey: ScriptPubKey) : TransactionOutput = factory(currencyUnit, scriptPubKey)

}