package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.gen.ZcashBlockchainElementGenerator
import org.scalacheck.{ Prop, Properties }

class ZcashBlockSpec extends Properties("ZcashBlockSpec") {

  property("block header serialization symmetry") = {
    Prop.forAll(ZcashBlockchainElementGenerator.blockHeader) { header =>
      ZcashBlockHeader.fromHex(header.hex) == header &&
        ZcashBlockHeader(header.version, header.previousBlockHash, header.merkleRootHash,
          header.hashReserved, header.time, header.nBits, header.nonceBytes,
          header.solution) == header
    }
  }

  property("zcash block serialization symmetry") = {
    Prop.forAll(ZcashBlockchainElementGenerator.zcashBlock) { block =>
      ZcashBlock.fromHex(block.hex) == block &&
        ZcashBlock(block.blockHeader, block.transactions) == block

    }
  }
}
