package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{ FlatSpec, MustMatchers }

class ZcashBlockTest extends FlatSpec with MustMatchers {
  // a raw block taken from a zcash regtest node, this is pre-overwinter
  val rawBlock = "0400000027e30134d620e9fe61f719938320bab63e7e72c91b5e23025676f90ed8119f025f9f1c8a5f5275ba5196eb975e88043006e592317548f8bc959979b930d1ce9c0000000000000000000000000000000000000000000000000000000000000000bab5d85a0f0f0f201700452579d2781beaac791760228e3b3073602524ca06b5a1216986f96f00002403b6593b42b5ccb53e12c9dc37421e90da9810aef4fc32e571619f136189b8f3936e5dac0101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff03510101ffffffff0200ca9a3b000000001976a914455a61c37227aa12b4fea74da5cb9e973c0c6ef088ac80b2e60e0000000017a9146708e6670db0b950dac68031025cc5b63213a4918700000000"
  val expectedBlockHash = BitcoinSUtil.flipEndianness("0e31ff00a6af07a601e6681521e9b3c5a5be20b2bb37eba088e9a4d96f204b60")
  "ZcashBlock" must "be able to parse a zcash block from a regtest node" in {
    val block = ZcashBlock(rawBlock)
    block.hex must be(rawBlock)

    block.blockHeader.hash.hex must be(expectedBlockHash)
  }
}
