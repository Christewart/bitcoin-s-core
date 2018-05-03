package org.bitcoins.core.gen

import org.bitcoins.core.protocol.blockchain.{ ZcashBlock, ZcashBlockHeader }
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

sealed abstract class ZcashBlockchainElementGenerator {

  def blockHeader: Gen[ZcashBlockHeader] = for {
    version <- NumberGenerator.uInt32s
    previousBlockHash <- CryptoGenerators.doubleSha256Digest
    merkleRoot <- CryptoGenerators.doubleSha256Digest
    time <- NumberGenerator.uInt32s
    nBits <- NumberGenerator.uInt32s
    hashReserved <- CryptoGenerators.sha256Digest
    solutionSize <- Gen.choose(1, 1344)
    solution <- Gen.listOfN(solutionSize, arbitrary[Byte])
    nonce <- Gen.listOfN(32, arbitrary[Byte])
  } yield ZcashBlockHeader(version, previousBlockHash, merkleRoot,
    hashReserved, time, nBits, nonce, solution)

  def zcashBlock: Gen[ZcashBlock] = for {
    header <- blockHeader
    txSize <- Gen.choose(1, 5)
    txs <- Gen.listOfN(txSize, ZcashTransactionGenerator.transaction)
  } yield ZcashBlock(header, txs)

}

object ZcashBlockchainElementGenerator extends ZcashBlockchainElementGenerator
