package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.protocol.transaction.{JSDescription, ZCProof}
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSatoshisSerializer}

/**
 * Created by str4d on 3/26/18.
 */
sealed abstract class RawJSDescriptionParser extends RawBitcoinSerializer[JSDescription] {

  override def read(bytes : List[Byte]) : JSDescription = {
    val vpubOldBytes = bytes.take(8)
    val vpubOld = RawSatoshisSerializer.read(vpubOldBytes)
    val vpubNewBytes = bytes.slice(8, 16)
    val vpubNew = RawSatoshisSerializer.read(vpubNewBytes)
    val anchor = bytes.slice(16, 48)
    val nullifiers = List(bytes.slice(48, 80), bytes.slice(80, 112))
    val commitments = List(bytes.slice(112, 144), bytes.slice(144, 176))
    val onetimePubKey = bytes.slice(176, 208)
    val randomSeed = bytes.slice(208, 240)
    val macs = List(bytes.slice(240, 272), bytes.slice(272, 304))
    val proofBytes = bytes.slice(304, bytes.size)
    val proof = ZCProof(proofBytes)
    val ciphertextBytes = proofBytes.slice(proof.size, proofBytes.size)
    val ciphertexts = List(ciphertextBytes.slice(0, 601), ciphertextBytes.slice(601, 1202))
    JSDescription(vpubOld,vpubNew,anchor,nullifiers,commitments,onetimePubKey,randomSeed,macs,proof,ciphertexts)
  }

  /** Writes a single JSDescription */
  def write(jsdesc : JSDescription): Seq[Byte] = {
    val vpubOld: Satoshis = CurrencyUnits.toSatoshis(jsdesc.vpubOld)
    val vpubNew: Satoshis = CurrencyUnits.toSatoshis(jsdesc.vpubNew)
    vpubOld.bytes ++
    vpubNew.bytes ++
    jsdesc.anchor ++
    jsdesc.nullifiers.flatten ++
    jsdesc.commitments.flatten ++
    jsdesc.onetimePubKey ++
    jsdesc.randomSeed ++
    jsdesc.macs.flatten ++
    jsdesc.proof.bytes ++
    jsdesc.ciphertexts.flatten
  }
}

object RawJSDescriptionParser extends RawJSDescriptionParser