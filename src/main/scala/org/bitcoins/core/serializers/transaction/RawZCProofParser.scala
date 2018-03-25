package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.UInt8
import org.bitcoins.core.protocol.transaction.ZCProof
import org.bitcoins.core.serializers.RawBitcoinSerializer

/**
 * Created by str4d on 3/26/18.
 */
sealed abstract class RawZCProofParser extends RawBitcoinSerializer[ZCProof] {
  val G1_PREFIX_MASK = 0x02
  val G2_PREFIX_MASK = 0x0a

  def readCompressedG1(bytes : List[Byte]) : Tuple2[Boolean, Seq[Byte]] = {
    val leadingByte = UInt8(bytes.take(1)).toInt
    require((leadingByte & (~1)) == G1_PREFIX_MASK, "lead byte of G1 point not recognized")
    Tuple2((leadingByte & 1) == 1, bytes.slice(1, 33))
  }

  def readCompressedG2(bytes : List[Byte]) : Tuple2[Boolean, Seq[Byte]] = {
    val leadingByte = UInt8(bytes.take(1)).toInt
    require((leadingByte & (~1)) == G2_PREFIX_MASK, "lead byte of G2 point not recognized")
    Tuple2((leadingByte & 1) == 1, bytes.slice(1, 65))
  }

  def writeCompressedG1(p : Tuple2[Boolean, Seq[Byte]]) : Seq[Byte] = {
    var leadingByte = G1_PREFIX_MASK | (if (p._1) 1 else 0)
    UInt8(leadingByte).bytes ++ p._2
  }

  def writeCompressedG2(p : Tuple2[Boolean, Seq[Byte]]) : Seq[Byte] = {
    var leadingByte = G2_PREFIX_MASK | (if (p._1) 1 else 0)
    UInt8(leadingByte).bytes ++ p._2
  }

  override def read(bytes : List[Byte]) : ZCProof = {
    val gA = readCompressedG1(bytes.slice(0, 33))
    val gAPrime = readCompressedG1(bytes.slice(33, 66))
    val gB = readCompressedG2(bytes.slice(66, 131))
    val gBPrime = readCompressedG1(bytes.slice(131, 164))
    val gC = readCompressedG1(bytes.slice(164, 197))
    val gCPrime = readCompressedG1(bytes.slice(197, 230))
    val gK = readCompressedG1(bytes.slice(230, 263))
    val gH = readCompressedG1(bytes.slice(263, 296))
    ZCProof(gA,gAPrime,gB,gBPrime,gC,gCPrime,gK,gH)
  }

  /** Writes a single ZCProof */
  def write(proof : ZCProof): Seq[Byte] = {
    writeCompressedG1(proof.gA) ++
    writeCompressedG1(proof.gAPrime) ++
    writeCompressedG2(proof.gB) ++
    writeCompressedG1(proof.gBPrime) ++
    writeCompressedG1(proof.gC) ++
    writeCompressedG1(proof.gCPrime) ++
    writeCompressedG1(proof.gK) ++
    writeCompressedG1(proof.gH)
  }
}

object RawZCProofParser extends RawZCProofParser