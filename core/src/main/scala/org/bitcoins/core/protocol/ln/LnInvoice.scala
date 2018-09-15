package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.number.{ UInt64, UInt8 }
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.util._

sealed abstract class LnInvoice {
  val bech32Separator: Char = Bech32Address.separator

  def hrp: LnHumanReadablePart

  def network: LnParams = hrp.network

  def amount: Option[LnCurrencyUnit] = hrp.amount

  def timestamp: UInt64

  def lnTags: LnInvoiceTags

  type Signature = (ECDigitalSignature, Int)
  def signature: Signature

  def bech32Checksum: String = "" //TODO: Unimplemented. See Bech32Address.createChecksum

  //TODO: Refactor Into Bech32Address?
  def uInt64ToBase32(input: UInt64): Seq[UInt8] = {
    //To fit a UInt64 value, we need at most ceil(64 / 5) = 13 groups of 5 bits.
    val arr: Array[Int] = new Array[Int](13)
    for (x <- 0 to 12) {
      arr(x) = (input >> x * 5 & UInt64(0x1F)).toInt.toByte
    }
    arr.reverse.dropWhile(_ == 0).map(b => UInt8(b))
  }

  def hexToBase32(hex: String): Seq[UInt8] = {
    val byteArray = BitcoinSUtil.decodeHex(hex).toSeq.map(b => UInt8(b))
    Bech32Address.encode(byteArray).get
  }

  def bech32TimeStamp: String = Bech32Address.encodeToString(uInt64ToBase32(timestamp))

  def bech32Signature: String = {
    val signatureHex = signature._1.hex + "%02d".format(signature._2) //Append version information
    val signatureBase32 = hexToBase32(signatureHex)
    Bech32Address.encodeToString(signatureBase32)
  }

  override def toString: String = { hrp.toString + bech32Separator + bech32TimeStamp + lnTags.toString + bech32Signature + bech32Checksum }
}

case class Invoice(hrp: LnHumanReadablePart, timestamp: UInt64, lnTags: LnInvoiceTags,
  signature: (ECDigitalSignature, Int)) extends LnInvoice