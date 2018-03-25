package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.transaction.RawJSDescriptionParser
import org.bitcoins.core.util.Factory

/**
 * Created by str4d on 3/26/18.
 */
sealed abstract class JSDescription extends NetworkElement {

  def vpubOld : CurrencyUnit
  def vpubNew : CurrencyUnit
  def anchor : Seq[Byte]
  def nullifiers : Seq[Seq[Byte]]
  def commitments : Seq[Seq[Byte]]
  def onetimePubKey : Seq[Byte]
  def randomSeed : Seq[Byte]
  def macs : Seq[Seq[Byte]]
  def proof : ZCProof
  def ciphertexts : Seq[Seq[Byte]]

  override def size = proof.size + 1506

  override def bytes = RawJSDescriptionParser.write(this)
}

object JSDescription extends Factory[JSDescription] {
  private case class JSDescriptionImpl(vpubOld : CurrencyUnit, vpubNew : CurrencyUnit, anchor : Seq[Byte],
                                       nullifiers : Seq[Seq[Byte]], commitments : Seq[Seq[Byte]],
                                       onetimePubKey : Seq[Byte], randomSeed : Seq[Byte],
                                       macs : Seq[Seq[Byte]], proof : ZCProof,
                                       ciphertexts : Seq[Seq[Byte]]) extends JSDescription

  def fromBytes(bytes : Seq[Byte]) : JSDescription = RawJSDescriptionParser.read(bytes)

  def apply(vpubOld : CurrencyUnit, vpubNew : CurrencyUnit, anchor : Seq[Byte],
            nullifiers : Seq[Seq[Byte]], commitments : Seq[Seq[Byte]],
            onetimePubKey : Seq[Byte], randomSeed : Seq[Byte],
            macs : Seq[Seq[Byte]], proof : ZCProof,
            ciphertexts : Seq[Seq[Byte]]) : JSDescription = {
    JSDescriptionImpl(vpubOld,vpubNew,anchor,nullifiers,commitments,onetimePubKey,randomSeed,macs,proof,ciphertexts)
  }
}