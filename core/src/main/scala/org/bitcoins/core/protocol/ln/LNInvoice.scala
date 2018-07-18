package org.bitcoins.core.protocol.ln

import org.bitcoins.core.config._
import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.currency.{ Bitcoins, CurrencyUnit, CurrencyUnits, Satoshis }
import org.bitcoins.core.number.UInt8
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.util._

sealed abstract class LightningInvoice {

  private def logger = BitcoinSLogger.logger

  val bech32Separator: Int = Bech32Address.separator

  def network: LightningNetworkParams

  //let's create currency units for lightning
  def amountOpt: Option[CurrencyUnit]

  def timestamp: Long

  def lnTags: LNInvoiceTags

  type Signature = (ECDigitalSignature, Int)
  def signature: Signature

  def bech32Checksum: String = "" //TODO: Unimplemented. See Bech32Address.createChecksum

  def multiplierOpt: Option[LnInvoiceMultiplier]

  def invoiceAmount: String = {
    (amountOpt, multiplierOpt) match {
      case (Some(amount), Some(multiplier)) =>
        currencyToString(amount) + multiplier.char
      case (Some(amount), None) =>
        currencyToString(amount)
      case (None, Some(multiplier)) =>
        throw new IllegalArgumentException(s"Cannot have a multipliler without an amount, got ${multiplier}")
      case (None, None) => ""
    }
  }

  private def currencyToString(amt: CurrencyUnit): String = {
    //not sure how to convert amount properly, just going to call `.toString()` which is wrong
    amt.satoshis.toLong.toString
  }

  def bech32TimeStamp: String = {
    val paddedBinary = timestamp.toBinaryString.reverse.padTo(35, "0").reverse.grouped(5).toList
    val timeBinaryToInt = paddedBinary.map(s => Integer.parseInt(s.mkString, 2))
    val timeUInt8 = timeBinaryToInt.map(i => UInt8(i.toShort))
    Bech32Address.encodeToString(timeUInt8)
  }

  def bech32EncodedSignature: String = {
    val sigRecoveryHex = if (signature._2 > 0) { "%02d".format(signature._2) } else { "" } //Hex value of recovery bit, padded to ##.
    val sigFullHex = signature._1.hex + sigRecoveryHex
    val sigHexPaddedBinary = BigInt(sigFullHex, 16).toString(2).reverse.padTo(512, "0").reverse.padTo(520, "0").grouped(5).toList //TODO: Fix this
    val sigHexBinaryToInt = sigHexPaddedBinary.map(s => Integer.parseInt(s.mkString, 2))
    val sigHexUInt8 = sigHexBinaryToInt.map(i => UInt8(i.toShort))
    Bech32Address.encodeToString(sigHexUInt8)
  }

  def invoice: String = { network.value + invoiceAmount + bech32Separator + bech32TimeStamp + lnTags.toBech32String + bech32EncodedSignature + bech32Checksum }

}

case class LNInvoice(
  network: LightningNetworkParams,
  amountOpt: Option[CurrencyUnit],
  multiplierOpt: Option[LnInvoiceMultiplier],
  lnTags: LNInvoiceTags,
  timestamp: Long,
  signature: (ECDigitalSignature, Int)) extends LightningInvoice

case class LNRoutingInfo(pubkey: String, shortChannelID: String, feeBaseMsat: Int, feePropMilli: Int, cltvExpiryDelta: Int) {

}

