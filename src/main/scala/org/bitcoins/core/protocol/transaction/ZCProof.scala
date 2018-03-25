package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.transaction.RawZCProofParser
import org.bitcoins.core.util.Factory

/**
 * Created by str4d on 3/26/18.
 */
sealed abstract class ZCProof extends NetworkElement {

  def gA : Tuple2[Boolean, Seq[Byte]]
  def gAPrime : Tuple2[Boolean, Seq[Byte]]
  def gB : Tuple2[Boolean, Seq[Byte]]
  def gBPrime : Tuple2[Boolean, Seq[Byte]]
  def gC : Tuple2[Boolean, Seq[Byte]]
  def gCPrime : Tuple2[Boolean, Seq[Byte]]
  def gK : Tuple2[Boolean, Seq[Byte]]
  def gH : Tuple2[Boolean, Seq[Byte]]

  override def size = 296

  override def bytes = RawZCProofParser.write(this)
}

object ZCProof extends Factory[ZCProof] {
  private case class ZCProofImpl(gA : Tuple2[Boolean, Seq[Byte]], gAPrime : Tuple2[Boolean, Seq[Byte]],
                                 gB : Tuple2[Boolean, Seq[Byte]], gBPrime : Tuple2[Boolean, Seq[Byte]],
                                 gC : Tuple2[Boolean, Seq[Byte]], gCPrime : Tuple2[Boolean, Seq[Byte]],
                                 gK : Tuple2[Boolean, Seq[Byte]], gH : Tuple2[Boolean, Seq[Byte]]) extends ZCProof

  def fromBytes(bytes : Seq[Byte]) : ZCProof = RawZCProofParser.read(bytes)

  def apply(gA : Tuple2[Boolean, Seq[Byte]], gAPrime : Tuple2[Boolean, Seq[Byte]],
            gB : Tuple2[Boolean, Seq[Byte]], gBPrime : Tuple2[Boolean, Seq[Byte]],
            gC : Tuple2[Boolean, Seq[Byte]], gCPrime : Tuple2[Boolean, Seq[Byte]],
            gK : Tuple2[Boolean, Seq[Byte]], gH : Tuple2[Boolean, Seq[Byte]]) : ZCProof = {
    ZCProofImpl(gA,gAPrime,gB,gBPrime,gC,gCPrime,gK,gH)
  }
}