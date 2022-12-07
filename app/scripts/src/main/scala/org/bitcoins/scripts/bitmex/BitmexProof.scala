package org.bitcoins.scripts.bitmex

import org.bitcoins.core.config.{NetworkParameters, Networks}
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.jdk.CollectionConverters.CollectionHasAsScala

case class BitmexProof(JBitmexProof: JBitmexProof) {
  val height: Long = JBitmexProof.getHeight

  val blockHash: DoubleSha256DigestBE =
    DoubleSha256DigestBE.fromHex(JBitmexProof.getBlockhash)
  val chain: NetworkParameters = Networks.fromString(JBitmexProof.getChain)
  val total: Satoshis = Satoshis(JBitmexProof.getTotal)
  val claim: BitmexClaim = BitmexClaim(JBitmexProof.getClaim)

  val xpub: Vector[ExtPublicKey] = {
    JBitmexProof.getXpub.asScala.toVector
      .map(ExtPublicKey.fromString)
  }

  val addresses: Vector[BitmexUtxo] = {
    JBitmexProof.getAddress.asScala.toVector
      .map(BitmexUtxo(_))
  }

  override def toString: String = {
    s"BitmexProof($blockHash,$chain,$total,$claim,$xpub,addressCount=${addresses.length})"
  }
}
