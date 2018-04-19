package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.protocol.blockchain.ZcashBlock
import org.bitcoins.core.protocol.transaction.ZcashTransaction
import org.bitcoins.core.serializers.{ RawBitcoinSerializer, RawSerializerHelper }

class RawZcashBlockSerializer extends RawBitcoinSerializer[ZcashBlock] {

  override def read(bytes: List[Byte]): ZcashBlock = {
    val header = RawZcashBlockHeaderSerializer.read(bytes)
    val txBytes = bytes.splitAt(header.size)._2
    val (transactions, _) = RawSerializerHelper.parseCmpctSizeUIntSeq[ZcashTransaction](txBytes, ZcashTransaction(_: Seq[Byte]))
    ZcashBlock(header, transactions)
  }

  override def write(block: ZcashBlock): Seq[Byte] = {
    RawBlockSerializer.write(block)
  }
}

object RawZcashBlockSerializer extends RawZcashBlockSerializer