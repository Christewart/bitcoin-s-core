package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.crypto.{ DoubleSha256Digest, Sha256Digest }
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.blockchain.ZcashBlockHeader
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSLogger

sealed abstract class RawZcashBlockHeaderSerializer extends RawBitcoinSerializer[ZcashBlockHeader] {
  private val logger = BitcoinSLogger.logger
  override def read(bytes: List[Byte]): ZcashBlockHeader = {
    val version = UInt32(bytes.take(4).reverse)
    //previous header hash next 32 bytes
    val prevBlockHashBytes = bytes.slice(4, 36)
    val prevBlockHash: DoubleSha256Digest = DoubleSha256Digest(prevBlockHashBytes)
    //merkle hash next 32 bytes
    val merkleRootBytes = bytes.slice(36, 68)
    val merkleRoot: DoubleSha256Digest = DoubleSha256Digest(merkleRootBytes)

    val hashReservedBytes = bytes.slice(68, 100)
    val hashReserved = Sha256Digest(hashReservedBytes)

    val timeBytes = bytes.slice(100, 104)
    val time = UInt32(timeBytes.reverse)

    val nBitsBytes = bytes.slice(104, 108)
    val nBits = UInt32(nBitsBytes.reverse)

    val nonceBytes = bytes.slice(108, 140)

    val rem = bytes.slice(140, bytes.size)

    val compactSizeUInt = CompactSizeUInt.parse(rem)
    val len = compactSizeUInt.num.toInt
    val solution = rem.slice(compactSizeUInt.size, len + compactSizeUInt.size)
    ZcashBlockHeader(version, prevBlockHash, merkleRoot, hashReserved,
      time, nBits, nonceBytes, solution)
  }

  override def write(blockHeader: ZcashBlockHeader): Seq[Byte] = {
    val version = blockHeader.version.bytes.reverse

    val prevHash = blockHeader.previousBlockHash.bytes
    val merkleRoot = blockHeader.merkleRootHash.bytes

    val time = blockHeader.time.bytes.reverse
    val nBits = blockHeader.nBits.bytes.reverse
    val nonce = blockHeader.nonceBytes
    val cmpctSolution = CompactSizeUInt.calc(blockHeader.solution)
    version ++ prevHash ++ merkleRoot ++ blockHeader.hashReserved.bytes ++
      time ++ nBits ++ nonce ++ cmpctSolution.bytes ++
      blockHeader.solution
  }

}

object RawZcashBlockHeaderSerializer extends RawZcashBlockHeaderSerializer
